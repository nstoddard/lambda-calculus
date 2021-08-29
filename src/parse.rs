use nom::branch::*;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::*;

use crate::types::*;

type VerboseError<'a> = nom::error::VerboseError<&'a str>;

fn alphanumeric_ident(i: &str) -> IResult<&str, Ident, VerboseError> {
    let (i, res): (_, Vec<char>) =
        many1(verify(anychar, |x| (x.is_alphanumeric() && *x != 'λ') || "'".contains(*x)))(i)?;
    Ok((i, res.into_iter().collect()))
}

fn symbolic_ident(i: &str) -> IResult<&str, Ident, VerboseError> {
    let (i, res): (_, Vec<char>) =
        many1(verify(anychar, |x| "!@#$%^&*-_=+,./<>?;:'\"[]{}\\|`~".contains(*x)))(i)?;
    Ok((i, res.into_iter().collect()))
}

const KEYWORDS: &[&str] =
    &["=", "help", "defs", "reset", "undefine", "->", "λ", "\\", ".", "simplify"];
const SUBSTITUTE_KEYWORDS: &[&str] = &[
    "equals",
    "help_",
    "defs_",
    "reset_",
    "undefine_",
    "arrow",
    "lambda",
    "backslash",
    "dot",
    "simplify_",
];

pub fn rename_keywords(ident: Ident) -> Ident {
    if let Some(index) = KEYWORDS.iter().position(|x| *x == ident) {
        SUBSTITUTE_KEYWORDS[index].to_owned()
    } else {
        ident
    }
}

// Identifiers are either alphanumeric, or contain only symbols.
fn ident(i: &str) -> IResult<&str, Ident, VerboseError> {
    verify(alt((alphanumeric_ident, symbolic_ident)), |ident| !KEYWORDS.contains(&ident))(i)
}

/// Attempts to parse the input as a function (except the parameter name, which is assumed to
/// have already been parsed)
fn try_parse_arrow_fn(i: &str) -> IResult<&str, Option<Expr<Ident>>, VerboseError> {
    opt(preceded(tuple((multispace0, tag("->"), multispace0)), parse_apply))(i)
}

fn parse_lambda_fn(i: &str) -> IResult<&str, Expr<Ident>, VerboseError> {
    let (i, params) = delimited(
        pair(alt((char('\\'), char('λ'))), multispace0),
        many1(terminated(ident, multispace0)),
        pair(char('.'), multispace0),
    )(i)?;
    let (i, fn_body) = parse_apply(i)?;
    assert!(!params.is_empty());
    let mut params = params.into_iter().rev();
    let mut res = Expr::f(params.next().unwrap(), fn_body);
    for param in params {
        res = Expr::f(param, res);
    }
    Ok((i, res))
}

fn parse_term(i: &str) -> IResult<&str, Expr<Ident>, VerboseError> {
    alt((
        delimited(pair(char('('), multispace0), parse_apply, pair(multispace0, char(')'))),
        parse_lambda_fn,
        map(pair(ident, try_parse_arrow_fn), |(ident, maybe_fn_body)| match maybe_fn_body {
            None => Expr::Var(ident),
            Some(fn_body) => Expr::f(ident, fn_body),
        }),
    ))(i)
}

fn parse_apply(i: &str) -> IResult<&str, Expr<Ident>, VerboseError> {
    // Function application is left-associative; e.g. "a b c" is equivalent to "(a b) c"
    let (i, f) = parse_term(i)?;
    fold_many0(preceded(multispace0, parse_term), move || f.clone(), Expr::apply)(i)
}

pub fn parse_def(i: &str) -> IResult<&str, (Ident, Expr<Ident>), VerboseError> {
    pair(ident, preceded(tuple((multispace0, char('='), multispace0)), parse_apply))(i)
}

pub fn parse_repl_command(i: &str) -> IResult<&str, ReplCommand, VerboseError> {
    alt((
        map(tag("defs"), |_| ReplCommand::PrintDefs),
        map(tag("help"), |_| ReplCommand::PrintHelp),
        map(tag("reset"), |_| ReplCommand::ResetDefs),
        map(
            preceded(
                tag("undefine"),
                fold_many0(preceded(multispace0, ident), Vec::new, |mut xs, x| {
                    xs.push(x);
                    xs
                }),
            ),
            ReplCommand::Undefine,
        ),
        map(preceded(tag("simplify"), preceded(multispace0, parse_apply)), ReplCommand::Simplify),
        map(parse_def, |(ident, expr)| ReplCommand::Def(ident, expr)),
        map(parse_apply, ReplCommand::Expr),
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

    fn var(x: &str) -> Expr<Ident> {
        Expr::Var(x.to_owned())
    }

    #[test]
    fn parse_idents() {
        assert_eq!(run_parser2(ident, "test"), Some("test".to_owned()));
        assert_eq!(run_parser2(ident, "test("), None);
        assert_eq!(run_parser2(ident, " "), None);
    }

    #[test]
    fn parse_apply_test() {
        assert_eq!(run_parser2(parse_apply, "test"), Some(var("test")));
        assert_eq!(run_parser2(parse_apply, "a b"), Some(Expr::apply(var("a"), var("b"))));
        assert_eq!(
            run_parser2(parse_apply, "a b c"),
            Some(Expr::apply(Expr::apply(var("a"), var("b")), var("c")))
        );
        assert_eq!(run_parser2(parse_apply, "(a b) c"), run_parser2(parse_apply, "a b c"));
        assert_eq!(run_parser2(parse_apply, "(a b) c)"), None);
        assert_eq!(
            run_parser2(parse_apply, "a (b c)"),
            Some(Expr::apply(var("a"), Expr::apply(var("b"), var("c"))))
        );
    }

    #[test]
    fn parse_arrow_syntax() {
        assert_eq!(run_parser2(parse_apply, "a -> a"), Some(Expr::f("a".to_owned(), var("a"))));
        assert_eq!(
            run_parser2(parse_apply, "a -> b -> a"),
            Some(Expr::f("a".to_owned(), Expr::f("b".to_owned(), var("a"))))
        );

        assert_eq!(
            run_parser2(parse_repl_command, "compose = f -> g -> x -> f (g x)"),
            Some(ReplCommand::Def(
                "compose".to_owned(),
                run_parser(parse_apply, "f -> g -> x -> f (g x)").unwrap()
            ))
        );
    }

    #[test]
    fn parse_lambda_syntax() {
        assert_eq!(run_parser2(parse_apply, "λa. a"), Some(Expr::f("a".to_owned(), var("a"))));
        assert_eq!(
            run_parser2(parse_apply, "λa b. a"),
            Some(Expr::f("a".to_owned(), Expr::f("b".to_owned(), var("a"))))
        );

        assert_eq!(
            run_parser2(parse_repl_command, "λa. a"),
            Some(ReplCommand::Expr(run_parser2(parse_apply, "a -> a").unwrap()))
        );
        assert_eq!(
            run_parser2(parse_repl_command, "compose = λf g x. f (g x)"),
            Some(ReplCommand::Def(
                "compose".to_owned(),
                run_parser(parse_apply, "λf g x. f (g x)").unwrap()
            ))
        );
        assert_eq!(run_parser2(parse_apply, "λa. a"), run_parser2(parse_apply, "\\a. a"));
        assert_eq!(run_parser2(parse_apply, "λa b. a"), run_parser2(parse_apply, "λa. λb. a"));
    }

    #[test]
    fn parse_repl_command_test() {
        assert_eq!(
            run_parser2(parse_repl_command, "a -> a"),
            Some(ReplCommand::Expr(run_parser2(parse_apply, "a -> a").unwrap()))
        );
        assert_eq!(
            run_parser2(parse_repl_command, "compose = f -> g -> x -> f (g x)"),
            Some(ReplCommand::Def(
                "compose".to_owned(),
                run_parser(parse_apply, "f -> g -> x -> f (g x)").unwrap()
            ))
        );
        assert_eq!(
            run_parser2(parse_repl_command, "undefine foo bar"),
            Some(ReplCommand::Undefine(vec!["foo".to_owned(), "bar".to_owned()]))
        );
        assert_eq!(
            run_parser2(parse_repl_command, "simplify foo"),
            Some(ReplCommand::Simplify(run_parser2(parse_apply, "foo").unwrap()))
        );
    }
}
