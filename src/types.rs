use std::cell::RefCell;
use std::rc::Rc;

pub type Index = u16;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement<Var> {
    Def(Ident, Expr<Var>),
    Expr(Expr<Var>),
    // TODO: these should be REPL commands, not statements
    PrintDefs,
    PrintHelp,
    ResetDefs,
    Undefine(Vec<Var>),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Expr<Var> {
    Fn(Ident, Box<Expr<Var>>),
    /// There are three representations of variables: identifiers (`Ident`), which are used when parsing and displaying expressions, De Bruijn indices (the `Var` enum), which are mainly used as an intermediate representation when converting to/from thunks, and thunks (the `ThunkVar` enum), used when evaluating expressions.
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ThunkVar {
    /// Free variable, or reference to definition
    Free(Ident),
    /// De Bruijn index
    Param(Index),
    /// A thunk. This isn't technically a variable, but is stored as a `Var` because otherwise an entirely separate `Expr` struct would be required when using thunks.
    Thunk(Thunk),
}

/// Thunks are a way to reduce duplicate work when evaluating expressions; they make the evaluation
/// of many expressions orders of magnitude faster, at the cost of implementation complexity.
// TODO: is `Thunk` the best name for this type?
pub type Thunk = Rc<RefCell<ThunkData>>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ThunkData {
    /// A thunk containing an expression that hasn't yet been evaluated
    // This is an `Option` to avoid needing to clone it in `eval()`
    Unevaluated(Option<Expr<ThunkVar>>),
    /// A thunk containing an evaluated expression
    Evaluated(Expr<ThunkVar>),
}
