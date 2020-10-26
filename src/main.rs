#![cfg(not(target_arch = "wasm32"))]

mod defs;
mod eval;
mod parse;
mod types;

use directories_next::*;
use rustyline::{error::*, At, Cmd, Editor, KeyPress, Movement, Word};
use std::collections::*;
use std::fs::{self, File};
use std::io::BufWriter;

use defs::*;
use parse::*;
use types::*;

const HISTORY_FILENAME: &str = "history.txt";
const DEFS_FILENAME: &str = "defs.txt";

const HELP_STR: &str = "Lambda Calculus interpreter, by Nathan Stoddard

Lambda Calculus is a simple model of computation, with the only data type being functions that take one argument and return one result. Despite its simplicity, it's Turing-complete. For more information about it, see the Wikipedia page (https://en.wikipedia.org/wiki/Lambda_calculus), the Wikibooks page (https://en.wikibooks.org/wiki/Programming_Languages/Semantics_Specification#The_Built-in_Operations_of_Lambda_Calculus), or many other sources.

This is a small project to experiment with lambda calculus. It's not intended to be useful in production or be feature-complete. It also has a slightly different syntax than standard lambda calculus ('a -> a' rather than 'λa. a') because I don't like the standard alternatives to the lambda character.

Syntax:
    Functions: 'a -> a'
    Function application: '(a -> b -> a) x y'
    Definitions: 'id = a -> a'
    
    Names can either be alphanumeric (and unlike in most languages, can start with a digit), or contain only symbols (most ASCII characters are allowed).
    Definitions are substituted into expressions before evaluation, so they can't be used for recursion.
    The output of evaluating an expression is typically displayed twice: a minimal form in terms of definitions where possible, followed by the full expression (if different).

Commands:
    help: display this help info
    defs: display everything that has been defined
    reset: remove all definitions
    undefine foo: remove the definition for 'foo'
";

fn print_expr(expr: Expr<Var>, defs_lookup: &HashMap<Expr<Var>, HashSet<Ident>>) {
    let min_expr = expr.clone().find_minimal_form(&defs_lookup);
    if min_expr != expr {
        println!("{}\t\t({})", min_expr.indices_to_idents(), expr.indices_to_idents());
    } else {
        println!("{}", expr.indices_to_idents());
    }
}

fn main() {
    let dirs = ProjectDirs::from("", "", "lambda-calculus").unwrap();
    let save_dir = dirs.data_dir();
    fs::create_dir_all(save_dir).unwrap();
    let history_file = save_dir.join(HISTORY_FILENAME);
    let defs_file = save_dir.join(DEFS_FILENAME);

    let mut editor = Editor::<()>::new();
    editor.bind_sequence(KeyPress::ControlLeft, Cmd::Move(Movement::BackwardWord(1, Word::Big)));
    editor.bind_sequence(
        KeyPress::ControlRight,
        Cmd::Move(Movement::ForwardWord(1, At::AfterEnd, Word::Big)),
    );
    if history_file.exists() {
        editor.load_history(&history_file).unwrap();
    }
    let mut defs = if defs_file.exists() {
        Defs::from_str(&fs::read_to_string(&defs_file).unwrap())
    } else {
        Defs::default()
    };

    if defs.is_default() || defs.is_empty() {
        println!("{}", HELP_STR);
    }

    loop {
        match editor.readline("> ") {
            Ok(line) => {
                if line.trim().is_empty() {
                    continue;
                }
                editor.add_history_entry(&line);
                match run_parser(parse_repl_command, &line) {
                    Ok(ReplCommand::Expr(expr)) => {
                        let res = expr
                            .idents_to_indices()
                            .substitute_defs(defs.ident_to_def())
                            .and_then(|expr| expr.into_lazy().eval())
                            .and_then(|expr| expr.into_non_lazy());
                        match res {
                            Ok(expr) => print_expr(expr, defs.def_to_ident()),
                            Err(err) => println!("{}", err),
                        }
                    }
                    Ok(ReplCommand::Def(ident, expr)) => {
                        defs.add(ident.clone(), expr);
                        let expr = defs[&ident].clone();
                        print_expr(expr, defs.def_to_ident());
                    }
                    Ok(ReplCommand::PrintDefs) => {
                        let defs = defs.accessible_defs();
                        if defs.is_empty() {
                            println!("Nothing has been defined yet.");
                        } else {
                            for def in defs {
                                println!("{}", def);
                            }
                        }
                    }
                    Ok(ReplCommand::PrintHelp) => {
                        println!("{}", HELP_STR);
                    }
                    Ok(ReplCommand::ResetDefs) => {
                        defs.reset();
                    }
                    Ok(ReplCommand::Undefine(xs)) => {
                        for x in xs {
                            if defs.undefine(&x) {
                                println!("Definition for '{}' removed.", x);
                            } else {
                                println!("'{}' isn't defined.", x);
                            }
                        }
                    }
                    Err(err) => println!(
                        "Syntax error: {}",
                        nom::error::convert_error(&line, get_nom_error(err))
                    ),
                }
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => panic!("{}", err),
        }
    }
    editor.save_history(&history_file).unwrap();

    if defs.modified() {
        let mut defs_file = BufWriter::new(File::create(&defs_file).unwrap());
        defs.save(&mut defs_file);
    }
}
