use serde::*;
use std::cell::RefCell;
use std::rc::Rc;

pub type Index = u16;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ReplCommand {
    Def(Ident, Expr<Ident>),
    Expr(Expr<Ident>),
    PrintDefs,
    PrintHelp,
    ResetDefs,
    Undefine(Vec<Ident>),
    Simplify(Expr<Ident>),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize, Deserialize)]
pub enum Expr<Var> {
    Fn(Ident, Box<Expr<Var>>),
    /// There are two representations of variables: identifiers (`Ident`), which are used when
    /// parsing and displaying expressions, and De Bruijn indices (the `Var` enum), which are used
    /// at most other times.
    Var(Var),
    Apply(Box<Expr<Var>>, Box<Expr<Var>>),
}

pub type Ident = String;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Var {
    /// Free variable, or reference to definition
    Free(Ident),
    /// De Bruijn index (starting at 0, not 1)
    Param(Index),
}

/// An `Expr` that uses thunks for lazy evaluation, resulting in a large performance increase.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LazyExpr {
    Fn(Ident, Box<LazyExpr>),
    Var(Var),
    Apply(Box<LazyExpr>, Box<LazyExpr>),
    Thunk(Rc<RefCell<Thunk>>),
}

/// Thunks are a way to reduce duplicate work when evaluating expressions; they make the evaluation
/// of many expressions orders of magnitude faster, at the cost of implementation complexity.
// TODO: is `Thunk` the best name for this type?
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Thunk {
    /// A thunk containing an expression that hasn't yet been evaluated
    // This is an `Option` to avoid needing to clone it in `eval()`
    Unevaluated(Option<LazyExpr>),
    /// A thunk containing an evaluated expression
    Evaluated(LazyExpr),
}
