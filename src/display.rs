use serde::*;
use std::fmt::{self, Write};

use crate::types::*;

#[derive(Copy, Clone, Serialize, Deserialize)]
pub enum ExprSyntax {
    Lambda,
    Arrow,
}

// The standard `Display` trait can't be used here because it doesn't have a way to specify one of
// several syntaxes.
impl Expr<Ident> {
    /// For a function with multiple arguments (i.e. something like "\a b. a"), this returns a tuple of
    /// (all parameters as a `Vec`, the function body).
    fn get_params(&self) -> (Vec<Ident>, &Self) {
        let mut params = vec![];
        let mut expr = self;
        while let Expr::Fn(ident, body) = expr {
            params.push(ident.clone());
            expr = body;
        }
        (params, expr)
    }

    pub fn display(&self, syntax: ExprSyntax) -> String {
        fn display_expr_inner(
            expr: &Expr<Ident>,
            syntax: ExprSyntax,
            res: &mut String,
        ) -> fmt::Result {
            match syntax {
                ExprSyntax::Lambda => match expr {
                    Expr::Fn { .. } => {
                        let (params, body) = expr.get_params();
                        write!(res, "λ")?;
                        let mut params = params.into_iter();
                        if let Some(param) = params.next() {
                            write!(res, "{}", param)?;
                        }
                        for param in params {
                            write!(res, " {}", param)?;
                        }
                        write!(res, ". ")?;
                        display_expr_inner(body, syntax, res)
                    }
                    Expr::Var(var) => write!(res, "{}", var),
                    Expr::Apply(function, arg) => {
                        let fn_needs_parens = matches!(function.as_ref(), Expr::Fn(_, _));
                        if fn_needs_parens {
                            write!(res, "(")?;
                        }
                        display_expr_inner(function, syntax, res)?;
                        if fn_needs_parens {
                            write!(res, ")")?;
                        }
                        write!(res, " ")?;
                        let arg_needs_parens =
                            matches!(arg.as_ref(), Expr::Fn(_, _) | Expr::Apply(_, _));
                        if arg_needs_parens {
                            write!(res, "(")?;
                        }
                        display_expr_inner(arg, syntax, res)?;
                        if arg_needs_parens {
                            write!(res, ")")?;
                        }
                        Ok(())
                    }
                },
                ExprSyntax::Arrow => match expr {
                    Expr::Fn(ident, body) => {
                        write!(res, "{} -> ", ident)?;
                        display_expr_inner(body, syntax, res)
                    }
                    Expr::Var(var) => write!(res, "{}", var),
                    Expr::Apply(function, arg) => {
                        let fn_needs_parens = matches!(function.as_ref(), Expr::Fn(_, _));
                        if fn_needs_parens {
                            write!(res, "(")?;
                        }
                        display_expr_inner(function, syntax, res)?;
                        if fn_needs_parens {
                            write!(res, ")")?;
                        }
                        write!(res, " ")?;
                        let arg_needs_parens =
                            matches!(arg.as_ref(), Expr::Fn(_, _) | Expr::Apply(_, _));
                        if arg_needs_parens {
                            write!(res, "(")?;
                        }
                        display_expr_inner(arg, syntax, res)?;
                        if arg_needs_parens {
                            write!(res, ")")?;
                        }
                        Ok(())
                    }
                },
            }
        }

        let mut res = String::new();
        display_expr_inner(self, syntax, &mut res).unwrap();
        res
    }
}
