use nom::branch::*;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::*;

use crate::types::*;

type VerboseError<'a> = nom::error::VerboseError<&'a str>;

fn ident1(i: &str) -> IResult<&str, Ident, VerboseError> {
    let (i, res): (_, Vec<char>) =
        many1(verify(anychar, |x| x.is_alphanumeric() || "'".contains(*x)))(i)?;
    Ok((i, res.into_iter().collect()))
}

fn ident2(i: &str) -> IResult<&str, Ident, VerboseError> {
    let (i, res): (_, Vec<char>) =
        many1(verify(anychar, |x| "!@#$%^&*-_=+,./<>?;:'\"[]{}\\|`~".contains(*x)))(i)?;
    Ok((i, res.into_iter().collect()))
}

const KEYWORDS: &[&str] = &["=", "->", "help", "defs", "reset", "undefine"];

// Identifiers are either alphanumeric, or contain only symbols.
fn ident(i: &str) -> IResult<&str, Ident, VerboseError> {
    verify(alt((ident1, ident2)), |ident| !KEYWORDS.contains(&ident))(i)
}

/// Attempts to parse the input as a function, aside from the argument
fn try_parse_fn(i: &str) -> IResult<&str, Option<Expr<Ident>>, VerboseError> {
    opt(preceded(tuple((multispace0, tag("->"), multispace0)), parse_apply))(i)
}

fn parse_term(i: &str) -> IResult<&str, Expr<Ident>, VerboseError> {
    alt((
        delimited(pair(char('('), multispace0), parse_apply, pair(multispace0, char(')'))),
        map(pair(ident, try_parse_fn), |(ident, maybe_fn_body)| match maybe_fn_body {
            None => Expr::Var(ident),
            Some(fn_body) => Expr::f(ident, fn_body),
        }),
    ))(i)
}

fn parse_apply(i: &str) -> IResult<&str, Expr<Ident>, VerboseError> {
    // Function application is left-associative; e.g. "a b c" is equivalent to "(a b) c"
    let (i, f) = parse_term(i)?;
    fold_many0(preceded(multispace0, parse_term), f, Expr::apply)(i)
}

pub fn parse_def(i: &str) -> IResult<&str, (Ident, Expr<Ident>), VerboseError> {
    pair(ident, preceded(tuple((multispace0, char('='), multispace0)), parse_apply))(i)
}

pub fn parse_statement(i: &str) -> IResult<&str, Statement<Ident>, VerboseError> {
    alt((
        map(tag("defs"), |_| Statement::PrintDefs),
        map(tag("help"), |_| Statement::PrintHelp),
        map(tag("reset"), |_| Statement::ResetDefs),
        map(
            preceded(
                tag("undefine"),
                fold_many0(preceded(multispace0, ident), vec![], |mut xs, x| {
                    xs.push(x);
                    xs
                }),
            ),
            Statement::Undefine,
        ),
        map(parse_def, |(ident, expr)| Statement::Def(ident, expr)),
        map(parse_apply, Statement::Expr),
    ))(i)
}

pub fn run_parser<T>(
    parser: impl Fn(&str) -> IResult<&str, T, VerboseError>,
    i: &str,
) -> Result<T, nom::Err<VerboseError>> {
    let (i, res) = complete(all_consuming(delimited(multispace0, parser, multispace0)))(i)?;
    debug_assert!(i.is_empty());
    Ok(res)
}

pub fn get_nom_error(error: Err<VerboseError>) -> VerboseError {
    match error {
        Err::Incomplete(_) => unreachable!(),
        Err::Error(err) => err,
        Err::Failure(err) => err,
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    fn run_parser2<T>(
        parser: impl Fn(&str) -> IResult<&str, T, VerboseError>,
        i: &str,
    ) -> Option<T> {
        run_parser(parser, i).ok()
    }

    pub fn parse_expr(i: &str) -> Option<Expr<Ident>> {
        run_parser2(parse_apply, i)
    }

    #[test]
    fn parse_test() {
        assert_eq!(run_parser2(ident, "test"), Some("test".to_owned()));
        assert_eq!(run_parser2(ident, "test("), None);
        assert_eq!(run_parser2(ident, " "), None);

        assert_eq!(run_parser2(parse_apply, "test"), Some(Expr::Var("test".to_owned())));
        assert_eq!(
            run_parser2(parse_apply, "a b"),
            Some(Expr::apply(Expr::Var("a".to_owned()), Expr::Var("b".to_owned())))
        );
        assert_eq!(
            run_parser2(parse_apply, "a b c"),
            Some(Expr::apply(
                Expr::apply(Expr::Var("a".to_owned()), Expr::Var("b".to_owned())),
                Expr::Var("c".to_owned())
            ))
        );
        assert_eq!(run_parser2(parse_apply, "(a b) c"), run_parser2(parse_apply, "a b c"),);
        assert_eq!(run_parser2(parse_apply, "(a b) c)"), None,);
        assert_eq!(
            run_parser2(parse_apply, "a (b c)"),
            Some(Expr::apply(
                Expr::Var("a".to_owned()),
                Expr::apply(Expr::Var("b".to_owned()), Expr::Var("c".to_owned())),
            ))
        );
        assert_eq!(
            run_parser2(parse_apply, "a -> a"),
            Some(Expr::f("a".to_owned(), Expr::Var("a".to_owned())))
        );

        assert_eq!(
            run_parser2(parse_statement, "a -> a"),
            Some(Statement::Expr(run_parser2(parse_apply, "a -> a").unwrap()))
        );
        assert_eq!(
            run_parser2(parse_statement, "0 = a -> b -> a"),
            Some(Statement::Def("0".to_owned(), run_parser(parse_apply, "a -> b -> a").unwrap()))
        );
    }
}
