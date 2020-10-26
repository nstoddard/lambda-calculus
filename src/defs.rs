use std::collections::*;
use std::fmt::{self, Display, Formatter};
use std::io::Write;

use crate::parse::*;
use crate::types::*;

pub struct Def(Ident, Expr<Ident>);

impl Display for Def {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} = ", self.0)?;
        self.1.fmt(f)
    }
}

pub struct Defs {
    ordered_defs: Vec<(Ident, Expr<Ident>)>,
    ident_to_def: HashMap<Ident, Expr<Var>>,
    // Multiple idents can have the same definition, so this needs to map to a set.
    def_to_ident: HashMap<Expr<Var>, HashSet<Ident>>,
    modified: bool,
    is_default: bool,
}

const DEFAULT_DEFS: &str = "id = a -> a\n. = f -> g -> x -> f (g x)";

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
        let ordered_defs: Vec<_> =
            input.lines().map(|line| run_parser(parse_def, line).unwrap()).collect();
        let ident_to_def: HashMap<_, _> = ordered_defs
            .iter()
            .cloned()
            .map(|(ident, expr)| (ident, expr.idents_to_indices()))
            .collect();
        let mut def_to_ident = HashMap::new();
        for (ident, expr) in &ident_to_def {
            insert_def_to_ident(&mut def_to_ident, expr.clone(), ident.clone());
        }
        // ident_to_def.iter().map(|(ident, expr)| (expr.clone(), ident.clone())).collect();
        Self {
            ordered_defs,
            ident_to_def,
            def_to_ident,
            modified: false,
            is_default: input.trim() == DEFAULT_DEFS,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.ordered_defs.is_empty()
    }

    pub fn is_default(&self) -> bool {
        self.is_default
    }

    pub fn add(&mut self, ident: String, expr: Expr<Ident>) {
        self.ordered_defs.push((ident.clone(), expr.clone()));
        let expr = expr.idents_to_indices();

        // This handles the case where you have two identifiers that are defined as the same expression.
        if let Some(old_def) = self.ident_to_def.get(&ident) {
            let idents = self.def_to_ident.get_mut(&old_def).unwrap();
            idents.remove(&ident);
            if idents.is_empty() {
                self.def_to_ident.remove(&old_def);
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
            self.ordered_defs.retain(|(ident2, _)| ident2 != ident);
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

    pub fn save(&self, writer: &mut impl Write) {
        let defs = self.accessible_defs();
        for def in defs {
            writeln!(writer, "{}", def).unwrap();
        }
    }

    /// Removes definitions that can no longer be accessed, and converts the results to `Def`s.
    pub fn accessible_defs(&self) -> Vec<Def> {
        let mut defined_idents = HashSet::new();
        let mut res: Vec<_> = self
            .ordered_defs
            .iter()
            .rev()
            .filter(|(ident, _)| {
                let already_defined = defined_idents.contains(ident);
                defined_idents.insert(ident.clone());
                !already_defined
            })
            .map(|(ident, expr)| Def(ident.clone(), expr.clone()))
            .collect();
        res.reverse();
        res
    }
}

impl std::ops::Index<&str> for Defs {
    type Output = Expr<Var>;

    fn index(&self, index: &str) -> &Expr<Var> {
        &self.ident_to_def[index]
    }
}
