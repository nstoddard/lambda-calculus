#![cfg(not(target_arch = "wasm32"))]

mod defs;
mod eval;
mod parse;
mod types;
mod help;

use directories_next::*;
use rustyline::{error::*, Editor};
use std::collections::*;
use std::fs::{self, File};
use std::io::BufWriter;
use yew::*;
use yew::virtual_dom::*;

use defs::*;
use parse::*;
use types::*;
use help::*;

const HISTORY_FILENAME: &str = "history.txt";
const DEFS_FILENAME: &str = "defs2.txt";
const OLD_DEFS_FILENAME: &str = "defs.txt";

/// Converts a Yew HTML value to a string, including only its text content but without any
/// additional formatting, except that link URLs are placed in parentheses after the link label.
fn html_to_text(x: Html) -> String {
    let mut res = String::new();
    html_to_text_inner(x, &mut res);
    res
}

fn html_to_text_inner(x: VNode, res: &mut String) {
    match x {
        VNode::VTag(tag) => {
            html_list_to_text(tag.children, res);
            for (attr, val) in tag.attributes.iter() {
                if attr == "href" {
                    res.push_str(&format!(" ({})", val));
                }
            }
        }
        VNode::VText(text) => res.push_str(&text.text),
        VNode::VComp(_) => (), // TODO
        VNode::VList(list) => html_list_to_text(list, res),
        VNode::VRef(_) => (), // TODO
    }
}

fn html_list_to_text(list: VList, res: &mut String) {
    for node in list.children {
        html_to_text_inner(node, res);
    }
}

fn print_expr(expr: Expr<Var>, defs_lookup: &HashMap<Expr<Var>, HashSet<Ident>>) {
    let min_expr = expr.clone().find_minimal_form(defs_lookup);
    if min_expr != expr {
        println!("{}\t\t({})", min_expr.indices_to_idents(), expr.indices_to_idents());
    } else {
        println!("{}", expr.indices_to_idents());
    }
}

fn print_help() {
    println!("{}", html_to_text(help_html()));
}

fn main() {
    let dirs = ProjectDirs::from("", "", "lambda-calculus").unwrap();
    let save_dir = dirs.data_dir();
    fs::create_dir_all(save_dir).unwrap();
    let history_file = save_dir.join(HISTORY_FILENAME);
    let defs_file = save_dir.join(DEFS_FILENAME);
    let old_defs_file = save_dir.join(OLD_DEFS_FILENAME);

    let mut editor = Editor::<()>::new();
    if history_file.exists() {
        editor.load_history(&history_file).unwrap();
    }
    let mut defs = if defs_file.exists() {
        Defs::from_str(&fs::read_to_string(&defs_file).unwrap())
    } else if old_defs_file.exists() {
        Defs::from_old_defs(&fs::read_to_string(&old_defs_file).unwrap())
    } else {
        Defs::default()
    };

    if defs.is_default() || defs.is_empty() {
        print_help();
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
                        print_help();
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
                        nom::error::convert_error(line.as_ref(), get_nom_error(err))
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
