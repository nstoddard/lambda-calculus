use hashbag::*;
use simple_error::*;
use std::cell::RefCell;
use std::collections::*;
use std::fmt::{self, Display, Formatter};
use std::rc::Rc;

use crate::types::*;

const SUBSTITUTE_DEFS_RECURSION_LIMIT: usize = 100;
const FIND_MINIMAL_FORM_RECURSION_LIMIT: usize = 100;

// These are slightly below the values that would result in a stack overflow. The limit is much
// lower in debug builds.
// These limits should be as high as possible because some lambda calculus expressions can become
// extremely deeply nested.
#[cfg(debug_assertions)]
const EVAL_RECURSION_LIMIT: u16 = 300;
#[cfg(not(debug_assertions))]
const EVAL_RECURSION_LIMIT: u16 = 3000;

const INTO_NON_LAZY_RECURSION_LIMIT: u16 = 200;

impl Display for Expr<Ident> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Fn(ident, body) => {
                write!(f, "{} -> ", ident)?;
                body.fmt(f)
            }
            Self::Var(var) => write!(f, "{}", var),
            Self::Apply(function, arg) => {
                let fn_needs_parens = matches!(function.as_ref(), Self::Fn(_, _));
                if fn_needs_parens {
                    write!(f, "(")?;
                }
                function.fmt(f)?;
                if fn_needs_parens {
                    write!(f, ")")?;
                }
                write!(f, " ")?;
                let arg_needs_parens = matches!(arg.as_ref(), Self::Fn(_, _) | Self::Apply(_, _));
                if arg_needs_parens {
                    write!(f, "(")?;
                }
                arg.fmt(f)?;
                if arg_needs_parens {
                    write!(f, ")")?;
                }
                Ok(())
            }
        }
    }
}

impl<Var> Expr<Var> {
    /// Shorthand for creating an `Expr::Fn` without needing to call `Box::new`.
    pub fn f(param: Ident, body: Self) -> Self {
        Self::Fn(param, Box::new(body))
    }

    /// Shorthand for creating an `Expr::Apply` without needing to call `Box::new`.
    pub fn apply(f: Self, arg: Self) -> Self {
        Self::Apply(Box::new(f), Box::new(arg))
    }
}

impl LazyExpr {
    /// Shorthand for creating a `LazyExpr::Fn` without needing to call `Box::new`.
    pub fn f(param: Ident, body: Self) -> Self {
        Self::Fn(param, Box::new(body))
    }

    /// Shorthand for creating a `LazyExpr::Apply` without needing to call `Box::new`.
    pub fn apply(f: Self, arg: Self) -> Self {
        Self::Apply(Box::new(f), Box::new(arg))
    }
}

impl Expr<Ident> {
    /// Convert the `Expr` to use De Bruijn indices
    pub fn idents_to_indices(self) -> Expr<Var> {
        let mut ident_to_index = vec![];
        self.idents_to_indices_inner(&mut ident_to_index)
    }

    fn idents_to_indices_inner(self, ident_to_index: &mut Vec<Ident>) -> Expr<Var> {
        match self {
            Self::Fn(param, body) => {
                ident_to_index.push(param.clone());
                let res = Expr::f(param, body.idents_to_indices_inner(ident_to_index));
                ident_to_index.pop();
                res
            }
            Self::Var(ident) => match ident_to_index.iter().rposition(|x| x == &ident) {
                Some(index) => Expr::Var(Var::Param((ident_to_index.len() - 1 - index) as Index)),
                None => Expr::Var(Var::Free(ident)),
            },
            Self::Apply(f, arg) => Expr::apply(
                f.idents_to_indices_inner(ident_to_index),
                arg.idents_to_indices_inner(ident_to_index),
            ),
        }
    }
}

fn disambiguate_var(var: &str, used_idents: &HashBag<Ident>) -> String {
    let count = used_idents.contains(var);
    if count == 0 {
        var.to_owned()
    } else {
        format!("{}_{}", var, count)
    }
}

impl Expr<Var> {
    fn free_vars(&self) -> HashSet<Ident> {
        let mut res = HashSet::new();
        self.free_vars_inner(&mut res);
        res
    }

    fn free_vars_inner(&self, res: &mut HashSet<Ident>) {
        match self {
            Self::Fn(_, body) => body.free_vars_inner(res),
            Self::Var(Var::Param(_)) => (),
            Self::Var(Var::Free(ident)) => {
                res.insert(ident.clone());
            }
            Self::Apply(f, arg) => {
                f.free_vars_inner(res);
                arg.free_vars_inner(res);
            }
        }
    }

    /// Looks for a form of the current expression with as much as possible replaced with
    /// definitions.
    pub fn find_minimal_form(mut self, defs_lookup: &HashMap<Expr<Var>, HashSet<Ident>>) -> Self {
        for _ in 0..FIND_MINIMAL_FORM_RECURSION_LIMIT {
            let mut made_changes = false;
            self = self.find_minimal_form_inner(defs_lookup, &mut made_changes);
            if !made_changes {
                break;
            }
        }
        self
    }

    fn find_minimal_form_inner(
        self,
        defs_lookup: &HashMap<Expr<Var>, HashSet<Ident>>,
        made_changes: &mut bool,
    ) -> Self {
        if let Some(idents) = defs_lookup.get(&self) {
            *made_changes = true;
            Self::Var(Var::Free(idents.iter().next().unwrap().to_owned()))
        } else {
            match self {
                Self::Fn(param, body) => {
                    Self::f(param, body.find_minimal_form_inner(defs_lookup, made_changes))
                }
                Self::Var(_) => self,
                Self::Apply(f, arg) => Self::apply(
                    f.find_minimal_form_inner(defs_lookup, made_changes),
                    arg.find_minimal_form_inner(defs_lookup, made_changes),
                ),
            }
        }
    }

    /// Converts from De Bruijn indices to regular identifiers (with disambiguation as needed)
    pub fn indices_to_idents(self) -> Expr<Ident> {
        let mut used_idents: HashBag<_> = self.free_vars().into_iter().collect();
        let mut index_to_ident = vec![];
        self.indices_to_idents_inner(&mut index_to_ident, &mut used_idents)
    }

    fn indices_to_idents_inner(
        self,
        index_to_ident: &mut Vec<Ident>,
        used_idents: &mut HashBag<Ident>,
    ) -> Expr<Ident> {
        match self {
            Self::Fn(param, body) => {
                let ident = disambiguate_var(&param, used_idents);
                index_to_ident.push(ident.clone());
                used_idents.insert(param.clone());
                let res = Expr::f(ident, body.indices_to_idents_inner(index_to_ident, used_idents));
                index_to_ident.pop();
                used_idents.remove(&param);
                res
            }
            Self::Var(Var::Param(index)) => {
                Expr::Var(index_to_ident[index_to_ident.len() - 1 - index as usize].clone())
            }
            Self::Var(Var::Free(ident)) => Expr::Var(ident),
            Self::Apply(f, arg) => Expr::apply(
                f.indices_to_idents_inner(index_to_ident, used_idents),
                arg.indices_to_idents_inner(index_to_ident, used_idents),
            ),
        }
    }

    /// Converts the `Expr` to a `LazyExpr`.
    pub fn into_lazy(self) -> LazyExpr {
        match self {
            Self::Fn(param, body) => LazyExpr::f(param, body.into_lazy()),
            Self::Var(Var::Param(index)) => LazyExpr::Var(Var::Param(index)),
            Self::Var(Var::Free(ident)) => LazyExpr::Var(Var::Free(ident)),
            Self::Apply(f, arg) => LazyExpr::apply(f.into_lazy(), arg.into_lazy()),
        }
    }

    /// Replaces all uses of definitions with the contents of the definitions.
    ///
    /// Returns an error if definitions are too deeply nested (which should only happen if they're
    /// recursive).
    pub fn substitute_defs(self, defs: &HashMap<Ident, Self>) -> anyhow::Result<Self> {
        let mut res = self.clone();
        for _ in 0..SUBSTITUTE_DEFS_RECURSION_LIMIT {
            let mut made_changes = false;
            res = res.substitute_defs_inner(defs, &mut made_changes)?;
            if !made_changes {
                return Ok(res);
            }
        }
        Err(anyhow::Error::new(simple_error!(
            "Recursion limit reached when expanding expression: {}",
            self.indices_to_idents()
        )))
    }

    fn substitute_defs_inner(
        self,
        defs: &HashMap<Ident, Self>,
        made_changes: &mut bool,
    ) -> anyhow::Result<Self> {
        Ok(match self {
            Self::Fn(param, body) if defs.contains_key(&param) => {
                let mut defs: HashMap<Ident, Self> = defs.clone();
                defs.remove(&param);
                Self::f(param, body.substitute_defs_inner(&defs, made_changes)?)
            }
            Self::Fn(param, body) => {
                Self::f(param, body.substitute_defs_inner(defs, made_changes)?)
            }
            Self::Var(Var::Free(ident)) => match defs.get(&ident) {
                None => Self::Var(Var::Free(ident)),
                Some(expr) => {
                    *made_changes = true;
                    expr.clone()
                }
            },
            Self::Var(Var::Param(_)) => self,
            Self::Apply(f, arg) => Self::apply(
                f.substitute_defs_inner(defs, made_changes)?,
                arg.substitute_defs_inner(defs, made_changes)?,
            ),
        })
    }
}

impl LazyExpr {
    /// Replaces the parameter at the given depth with the given thunk.
    fn substitute(self, depth: Index, thunk: &Rc<RefCell<Thunk>>) -> Self {
        match self {
            Self::Fn(param, body) => Self::f(param, body.substitute(depth + 1, thunk)),
            Self::Var(Var::Param(depth2)) if depth2 == depth => Self::Thunk(thunk.clone()),
            expr @ Self::Var(_) => expr,
            expr @ Self::Thunk(_) => expr,
            Self::Apply(f, arg) => {
                Self::apply(f.substitute(depth, thunk), arg.substitute(depth, thunk))
            }
        }
    }

    /// Evaluates the expression.
    pub fn eval(self) -> anyhow::Result<Self> {
        self.eval_inner(0)
    }

    fn eval_inner(self, depth: u16) -> anyhow::Result<Self> {
        if depth >= EVAL_RECURSION_LIMIT {
            return Err(anyhow::Error::new(simple_error!(
                "Recursion limit reached when evaluating expression."
            )));
        }
        Ok(match self {
            expr @ Self::Fn(_, _) => expr,
            Self::Thunk(thunk) => {
                // This `borrow_mut()` is fine despite the recursive call to `eval_inner()` because
                // no thunk can contain itself, so it can't try to borrow this thunk twice.
                let thunk = &mut *thunk.borrow_mut();
                match thunk {
                    Thunk::Evaluated(expr) => expr.clone(),
                    Thunk::Unevaluated(expr) => {
                        let new_expr = expr.take().unwrap().eval_inner(depth+1)?;
                        *thunk = Thunk::Evaluated(new_expr.clone());
                        new_expr
                    }
                }
            }
            Self::Var(Var::Param(_)) => unreachable!(),
            expr @ Self::Var(Var::Free(_)) => expr,
            Self::Apply(f, arg) => match f.eval_inner(depth + 1)? {
                Self::Fn(_, body) => {
                    let thunk = Rc::new(RefCell::new(Thunk::Unevaluated(Some(*arg))));
                    let body = body.substitute(0, &thunk);
                    body.eval_inner(depth + 1)?
                }
                Self::Var(Var::Param(_)) => unreachable!(),
                Self::Var(Var::Free(ident)) => {
                    return Err(anyhow::Error::new(simple_error!(
                        "Can't apply free variable '{}' to argument '{}'",
                        ident,
                        arg.into_non_lazy()?.indices_to_idents()
                    )))
                }
                Self::Thunk(_) => unreachable!(),
                apply @ Self::Apply(_, _) => {
                    Self::apply(apply.eval_inner(depth + 1)?, *arg).eval_inner(depth + 1)?
                }
            },
        })
    }

    /// Removes thunks.
    pub fn into_non_lazy(self) -> anyhow::Result<Expr<Var>> {
        self.into_non_lazy_inner(0)
    }

    fn into_non_lazy_inner(self, depth: u16) -> anyhow::Result<Expr<Var>> {
        if depth >= INTO_NON_LAZY_RECURSION_LIMIT {
            return Err(anyhow::Error::new(simple_error!(
                "Recursion limit reached when converting expression to be displayed."
            )));
        }
        Ok(match self {
            Self::Fn(param, body) => Expr::f(param, body.into_non_lazy_inner(depth + 1)?),
            Self::Var(var) => Expr::Var(var),
            Self::Thunk(thunk) => match &*thunk.borrow() {
                Thunk::Unevaluated(expr) => expr.clone().unwrap().into_non_lazy_inner(depth + 1)?,
                Thunk::Evaluated(expr) => expr.clone().into_non_lazy_inner(depth + 1)?,
            },
            Self::Apply(f, arg) => {
                Expr::apply(f.into_non_lazy_inner(depth + 1)?, arg.into_non_lazy_inner(depth + 1)?)
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::tests::parse_expr;

    fn eval_expr(i: &str) -> Option<Expr<Var>> {
        parse_expr(i)
            .unwrap()
            .idents_to_indices()
            .into_lazy()
            .eval()
            .ok()
            .and_then(|expr| expr.into_non_lazy().ok())
    }

    // TODO: add more tests
    #[test]
    fn eval_test() {
        assert_eq!(eval_expr("(a -> a) x"), eval_expr("x"));
        assert_eq!(eval_expr("(a -> b -> a) x y"), eval_expr("x"));
        assert_eq!(eval_expr("(a -> b -> b) x y"), eval_expr("y"));

        assert_eq!(eval_expr("(a -> a a) (b -> b)"), eval_expr("b -> b"));

        assert_eq!(eval_expr("(a -> x) y"), eval_expr("x"));

        assert_eq!(eval_expr("(a -> a a) (b -> b)"), eval_expr("b -> b"));

        assert_eq!(eval_expr("(a -> a a) (a -> a a)"), None);
    }
}
