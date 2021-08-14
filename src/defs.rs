use serde::*;
use std::collections::*;
use std::fmt::{self, Display, Formatter};
use std::io::Write;

use crate::parse::*;
use crate::types::*;

#[derive(Serialize, Deserialize, Clone)]
pub struct Def {
    ident: Ident,
    val: Expr<Ident>,
}

impl Display for Def {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} = ", self.ident)?;
        self.val.fmt(f)
    }
}

pub struct Defs {
    ordered_defs: Vec<Def>,
    ident_to_def: HashMap<Ident, Expr<Var>>,
    // Multiple idents can have the same definition, so this needs to map to a set.
    def_to_ident: HashMap<Expr<Var>, HashSet<Ident>>,
    modified: bool,
    is_default: bool,
}

const DEFAULT_DEFS: &str = include_str!("../default_defs.txt");

impl Default for Defs {
    fn default() -> Self {
        Self::from_str(DEFAULT_DEFS)
    }
}

fn insert_def_to_ident(
    def_to_ident: &mut HashMap<Expr<Var>, HashSet<Ident>>,
    expr: Expr<Var>,
    ident: Ident,
) {
    if !def_to_ident.contains_key(&expr) {
        def_to_ident.insert(expr.clone(), HashSet::new());
    }
    def_to_ident.get_mut(&expr).unwrap().insert(ident);
}

impl Defs {
    pub fn from_str(input: &str) -> Self {
        let ordered_defs: Vec<Def> = serde_json::from_str(input).unwrap();
        Self::from_ordered_defs(ordered_defs, false, input.trim() == DEFAULT_DEFS)
    }

    pub fn from_old_defs(input: &str) -> Self {
        let ordered_defs: Vec<_> = input
            .lines()
            .map(|line| {
                let (ident, val) = run_parser(parse_def, line).unwrap();
                Def { ident, val }
            })
            .collect();
        Self::from_ordered_defs(ordered_defs, true, true)
    }

    fn from_ordered_defs(ordered_defs: Vec<Def>, modified: bool, is_default: bool) -> Self {
        let ident_to_def: HashMap<_, _> = ordered_defs
            .iter()
            .cloned()
            .map(|def| (def.ident, def.val.idents_to_indices()))
            .collect();
        let mut def_to_ident = HashMap::new();
        for (ident, expr) in &ident_to_def {
            insert_def_to_ident(&mut def_to_ident, expr.clone(), ident.clone());
        }
        Self { ordered_defs, ident_to_def, def_to_ident, modified, is_default }
    }

    pub fn save(&self, writer: &mut impl Write) {
        let defs = self.accessible_defs();
        serde_json::to_writer(writer, &defs).unwrap();
    }

    /// Removes definitions that can no longer be accessed, and converts the results to `Def`s.
    pub fn accessible_defs(&self) -> Vec<Def> {
        let mut defined_idents = HashSet::new();
        let mut res: Vec<_> = self
            .ordered_defs
            .iter()
            .rev()
            .filter(|def| {
                let already_defined = defined_idents.contains(&def.ident);
                defined_idents.insert(def.ident.clone());
                !already_defined
            })
            .cloned()
            .collect();
        res.reverse();
        res
    }

    pub fn is_empty(&self) -> bool {
        self.ordered_defs.is_empty()
    }

    pub fn is_default(&self) -> bool {
        self.is_default
    }

    pub fn add(&mut self, ident: String, expr: Expr<Ident>) {
        self.ordered_defs.push(Def { ident: ident.clone(), val: expr.clone() });
        let expr = expr.idents_to_indices();

        // This handles the case where you have two identifiers that are defined as the same expression.
        if let Some(old_def) = self.ident_to_def.get(&ident) {
            let idents = self.def_to_ident.get_mut(old_def).unwrap();
            idents.remove(&ident);
            if idents.is_empty() {
                self.def_to_ident.remove(old_def);
            }
        }

        self.ident_to_def.insert(ident.clone(), expr.clone());
        insert_def_to_ident(&mut self.def_to_ident, expr, ident);
        self.modified = true;
        self.is_default = false;
    }

    pub fn undefine(&mut self, ident: &str) -> bool {
        if let Some(old_def) = self.ident_to_def.remove(ident) {
            let idents = self.def_to_ident.get_mut(&old_def).unwrap();
            idents.remove(ident);
            if idents.is_empty() {
                self.def_to_ident.remove(&old_def);
            }
            self.ordered_defs.retain(|def| def.ident != ident);
            self.modified = true;
            self.is_default = false;
            true
        } else {
            false
        }
    }

    pub fn ident_to_def(&self) -> &HashMap<Ident, Expr<Var>> {
        &self.ident_to_def
    }

    pub fn def_to_ident(&self) -> &HashMap<Expr<Var>, HashSet<Ident>> {
        &self.def_to_ident
    }

    pub fn modified(&self) -> bool {
        self.modified
    }

    pub fn reset(&mut self) {
        *self = Defs::default();
        self.modified = true;
    }
}

impl std::ops::Index<&str> for Defs {
    type Output = Expr<Var>;

    fn index(&self, index: &str) -> &Expr<Var> {
        &self.ident_to_def[index]
    }
}
