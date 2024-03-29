#![cfg(target_arch = "wasm32")]
#![recursion_limit = "1024"]

mod defs;
mod display;
mod eval;
mod help;
mod parse;
mod types;

use std::cell::RefCell;
use std::collections::*;
use std::rc::Rc;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{HtmlElement, HtmlInputElement};
use yew::prelude::*;
use yew::services::storage::*;
use yew::utils::*;

use defs::*;
use display::*;
use help::*;
use parse::*;
use types::*;

const EXPR_TRUNCATE_LEN: usize = 200;
const MAX_HISTORY_LEN: usize = 500;

#[derive(Debug)]
enum Msg {
    ModifyCurStatement(String),
    KeyDown(KeyboardEvent),
    ToggleExprExpanded { i: usize, min: bool },
    ShowHelp,
    SwitchSyntax,
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum ExpandedState {
    AlwaysExpanded,
    NotExpanded,
    Expanded,
}

struct DisplayedExpr {
    expr: String,
    expanded_state: ExpandedState,
}

impl DisplayedExpr {
    fn new(expr: String) -> Self {
        Self {
            expanded_state: if expr.chars().count() > EXPR_TRUNCATE_LEN {
                ExpandedState::NotExpanded
            } else {
                ExpandedState::AlwaysExpanded
            },
            expr,
        }
    }

    fn to_html(
        &self,
        i: usize,
        min: bool,
        second_displayed_expr: bool,
        link: &ComponentLink<LambdaCalculus>,
    ) -> Html {
        match self.expanded_state {
            ExpandedState::AlwaysExpanded => html! { &self.expr },
            _ => {
                let mut classes = if self.expanded_state == ExpandedState::Expanded {
                    "expandButton expandButtonExpanded"
                } else {
                    "expandButton expandButtonNotExpanded"
                }
                .to_owned();
                if second_displayed_expr {
                    classes = format!("{} marginTop", classes);
                }
                html! {
                    <>
                        <button
                            class=classes
                            onclick=link.callback(move |_| Msg::ToggleExprExpanded{i, min})>
                            { if self.expanded_state == ExpandedState::Expanded {"▼"} else {"▶"} }
                        </button>
                        { if self.expanded_state == ExpandedState::NotExpanded {
                            format!("{}...", &self.expr.chars().take(EXPR_TRUNCATE_LEN).collect::<String>())
                        } else {
                            self.expr.clone()
                        } }
                    </>
                }
            }
        }
    }
}

enum EvalResult {
    Expr { min: Option<DisplayedExpr>, full: DisplayedExpr, input: String },
    Err { input: Option<String>, err: String },
    Help,
    Info(String),
}

impl EvalResult {
    fn new(
        input: String,
        expr: Expr<Var>,
        defs_lookup: &HashMap<Expr<Var>, HashSet<Ident>>,
        syntax: ExprSyntax,
    ) -> Self {
        let min_expr = expr.clone().find_minimal_form(defs_lookup);
        if min_expr != expr {
            EvalResult::Expr {
                input,
                min: Some(DisplayedExpr::new(min_expr.indices_to_idents().display(syntax))),
                full: DisplayedExpr::new(expr.indices_to_idents().display(syntax)),
            }
        } else {
            EvalResult::Expr {
                input,
                min: None,
                full: DisplayedExpr::new(expr.indices_to_idents().display(syntax)),
            }
        }
    }

    fn to_html(&self, i: usize, link: &ComponentLink<LambdaCalculus>) -> Html {
        match self {
            EvalResult::Err { input: Some(input), err } => html! {
                <div class="row">
                    <div class="monospace box input">{input}</div>
                    <div class="monospace large">{"→"}</div>
                    <div class="monospace error box">{err}</div>
                </div>
            },
            EvalResult::Err { input: None, err } => html! {
                <div class="monospace error box">{err}</div>
            },
            EvalResult::Info(info) => html! {
                <div class="box info shrinkBorder">{info}</div>
            },
            EvalResult::Help => help_html(),
            EvalResult::Expr { min, full, input } => html! {
                <div class="row">
                    <div class="monospace box input">{input}</div>
                    <div class="monospace large">{"→"}</div>
                    { match min {
                        None => html! {
                            <div class="monospace box row2">{full.to_html(i, false, false, link)}</div>
                        },
                        Some(min) => html! {
                            <div class="box">
                                <div class="monospace row2">{
                                    min.to_html(i, true, false, link)
                                }</div>
                                <div class="monospace dim row2">{
                                    full.to_html(i, false, true, link)
                                }</div>
                            </div>
                        }
                    } }
                </div>
            },
        }
    }
}

struct PersistentData {
    defs: Defs,
    history: Vec<String>,
    history_modified: bool,
    syntax: ExprSyntax,
    syntax_modified: bool,
}

impl PersistentData {
    fn save(&self, storage: &mut StorageService) {
        if self.defs.modified() {
            let mut defs_storage = Vec::new();
            self.defs.save(&mut defs_storage);
            storage.store("defs2", Ok(String::from_utf8(defs_storage).unwrap()));
        }
        if self.history_modified {
            storage.store("history", Ok(self.history.join("\n")));
        }
        if self.syntax_modified {
            storage.store("syntax", Ok(serde_json::to_string(&self.syntax).unwrap()));
        }
    }
}

struct LambdaCalculus {
    link: ComponentLink<Self>,
    cur_statement: String,

    eval_results: Vec<EvalResult>,

    shown_history_entry: Option<(usize, String)>,

    // This is in a `RefCell` because it must be accessible from the unload handler
    persistent_data: Rc<RefCell<PersistentData>>,

    scroll_defs: bool,
    scroll_repl: bool,
    // Used when replacing backslashes with lambdas
    selection_range: Option<(u32, u32)>,
}

impl Component for LambdaCalculus {
    type Message = Msg;
    type Properties = ();

    fn create(_: Self::Properties, link: ComponentLink<Self>) -> Self {
        let mut storage = StorageService::new(Area::Local).unwrap();

        let defs = if let Ok(input) = storage.restore("defs2") {
            Defs::from_str(&input)
        } else if let Ok(input) = storage.restore("defs") {
            Defs::from_old_defs(&input)
        } else {
            Defs::default()
        };
        let history = if let Ok(input) = storage.restore("history") {
            input.lines().map(|line| line.to_owned()).collect()
        } else {
            vec![]
        };
        let syntax = if let Ok(input) = storage.restore("syntax") {
            serde_json::from_str(&input).unwrap()
        } else {
            ExprSyntax::Lambda
        };

        let mut eval_results = vec![];
        if defs.is_default() || defs.is_empty() {
            eval_results.push(EvalResult::Help);
        }

        let persistent_data = Rc::new(RefCell::new(PersistentData {
            defs,
            history,
            history_modified: false,
            syntax,
            syntax_modified: false,
        }));
        let persistent_data2 = persistent_data.clone();

        let unload_handler = Closure::wrap(Box::new(move || {
            persistent_data2.borrow().save(&mut storage);
        }) as Box<dyn FnMut()>);
        window()
            .add_event_listener_with_callback("unload", unload_handler.as_ref().unchecked_ref())
            .unwrap();
        unload_handler.forget();

        Self {
            link,
            persistent_data,
            cur_statement: "".to_owned(),
            eval_results,
            shown_history_entry: None,
            scroll_defs: true,
            scroll_repl: true,
            selection_range: None,
        }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        match msg {
            Msg::ModifyCurStatement(statement) => {
                let has_backslash = statement.contains('\\');
                self.cur_statement = statement.replace('\\', "λ");
                // When an input element is modified by code, the cursor is normally moved to the
                // end of the input. This code, along with the corresponding code in `rendered()`,
                // prevents that from happening.
                if has_backslash {
                    let input_element = document()
                        .get_element_by_id("statementInput")
                        .unwrap()
                        .dyn_into::<HtmlInputElement>()
                        .unwrap();
                    self.selection_range = Some((
                        input_element.selection_start().unwrap().unwrap(),
                        input_element.selection_end().unwrap().unwrap(),
                    ));
                }
                true
            }
            Msg::ToggleExprExpanded { i, min } => {
                if let EvalResult::Expr { min: min_expr, full: full_expr, .. } =
                    &mut self.eval_results[i]
                {
                    let displayed_expr = if min { min_expr.as_mut().unwrap() } else { full_expr };
                    displayed_expr.expanded_state = match displayed_expr.expanded_state {
                        ExpandedState::AlwaysExpanded => unreachable!(),
                        ExpandedState::Expanded => ExpandedState::NotExpanded,
                        ExpandedState::NotExpanded => ExpandedState::Expanded,
                    };
                    true
                } else {
                    unreachable!()
                }
            }
            Msg::KeyDown(key) if key.key() == "ArrowUp" => {
                key.prevent_default();
                let persistent_data = self.persistent_data.borrow();
                let history = &persistent_data.history;
                if history.is_empty() {
                    false
                } else {
                    match &mut self.shown_history_entry {
                        None => {
                            self.shown_history_entry = Some((
                                history.len() - 1,
                                std::mem::replace(
                                    &mut self.cur_statement,
                                    history[history.len() - 1].clone(),
                                ),
                            ));
                            true
                        }
                        Some((i, _)) => {
                            if *i > 0 {
                                *i -= 1;
                                self.cur_statement = history[*i].clone();
                                true
                            } else {
                                false
                            }
                        }
                    }
                }
            }
            Msg::KeyDown(key) if key.key() == "ArrowDown" => {
                key.prevent_default();
                let persistent_data = self.persistent_data.borrow();
                let history = &persistent_data.history;
                match &mut self.shown_history_entry {
                    None => false,
                    Some((i, old_statement)) => {
                        if *i == history.len() - 1 {
                            self.cur_statement = std::mem::take(old_statement);
                            self.shown_history_entry = None;
                            true
                        } else {
                            *i += 1;
                            self.cur_statement = history[*i].clone();
                            true
                        }
                    }
                }
            }
            Msg::KeyDown(key) if key.key() == "Enter" => {
                let input = std::mem::replace(&mut self.cur_statement, "".to_owned());
                if input.trim().is_empty() {
                    return true;
                }
                self.shown_history_entry = None;
                self.scroll_repl = true;
                {
                    let mut persistent_data = self.persistent_data.borrow_mut();
                    if persistent_data.history.is_empty()
                        || persistent_data.history[persistent_data.history.len() - 1] != input
                    {
                        persistent_data.history.push(input.clone());
                        persistent_data.history_modified = true;
                        if persistent_data.history.len() > MAX_HISTORY_LEN {
                            persistent_data.history.remove(0);
                        }
                    }
                }
                match run_parser(parse_repl_command, &input) {
                    Ok(ReplCommand::Expr(expr)) => {
                        let syntax = self.persistent_data.borrow().syntax;
                        let res = expr
                            .idents_to_indices()
                            .substitute_defs(
                                self.persistent_data.borrow().defs.ident_to_def(),
                                syntax,
                            )
                            .and_then(|expr| expr.into_lazy().eval(syntax))
                            .and_then(|expr| expr.into_non_lazy());
                        self.eval_results.push(match res {
                            Ok(expr) => EvalResult::new(
                                input,
                                expr,
                                self.persistent_data.borrow().defs.def_to_ident(),
                                self.persistent_data.borrow().syntax,
                            ),
                            Err(err) => {
                                EvalResult::Err { input: Some(input), err: format!("{}", err) }
                            }
                        })
                    }
                    Ok(ReplCommand::Simplify(expr)) => {
                        let syntax = self.persistent_data.borrow().syntax;
                        let res = expr
                            .idents_to_indices()
                            .substitute_defs(
                                self.persistent_data.borrow().defs.ident_to_def(),
                                syntax,
                            )
                            .and_then(|expr| expr.into_lazy().eval(syntax))
                            .and_then(|expr| expr.into_non_lazy())
                            .and_then(|expr| expr.simplify());
                        self.eval_results.push(match res {
                            Ok(expr) => EvalResult::new(
                                input,
                                expr,
                                self.persistent_data.borrow().defs.def_to_ident(),
                                self.persistent_data.borrow().syntax,
                            ),
                            Err(err) => {
                                EvalResult::Err { input: Some(input), err: format!("{}", err) }
                            }
                        })
                    }
                    Ok(ReplCommand::Def(ident, expr)) => {
                        self.scroll_defs = true;
                        let mut persistent_data = self.persistent_data.borrow_mut();
                        let syntax = persistent_data.syntax;
                        let defs = &mut persistent_data.defs;
                        defs.add(ident.clone(), expr);
                        let expr = defs[&ident].clone();
                        let displayed_expr =
                            EvalResult::new(input, expr, defs.def_to_ident(), syntax);
                        self.eval_results.push(displayed_expr);
                    }
                    Ok(ReplCommand::PrintDefs) => {
                        self.eval_results.push(EvalResult::Err {
                            input: Some(input),
                            err:
                                "Not supported in wasm version (definitions are listed in the left column)"
                                    .to_owned(),
                        });
                    }
                    Ok(ReplCommand::PrintHelp) => {
                        self.eval_results.push(EvalResult::Help);
                    }
                    Ok(ReplCommand::ResetDefs) => {
                        self.persistent_data.borrow_mut().defs.reset();
                    }
                    Ok(ReplCommand::Undefine(xs)) => {
                        let mut persistent_data = self.persistent_data.borrow_mut();
                        for x in xs {
                            if persistent_data.defs.undefine(&x) {
                                self.eval_results.push(EvalResult::Info(format!(
                                    "Definition for '{}' removed.",
                                    x
                                )));
                            } else {
                                self.eval_results.push(EvalResult::Err {
                                    input: None,
                                    err: format!("'{}' isn't defined.", x),
                                });
                            }
                        }
                    }
                    Err(err) => self.eval_results.push(EvalResult::Err {
                        input: Some(input.clone()),
                        err: format!(
                            "Syntax error: {}",
                            nom::error::convert_error(input.as_ref(), get_nom_error(err))
                        ),
                    }),
                }
                true
            }
            Msg::KeyDown(_) => false,
            Msg::ShowHelp => {
                self.eval_results.push(EvalResult::Help);
                true
            }
            Msg::SwitchSyntax => {
                let syntax = &mut self.persistent_data.borrow_mut().syntax;
                *syntax = match syntax {
                    ExprSyntax::Lambda => ExprSyntax::Arrow,
                    ExprSyntax::Arrow => ExprSyntax::Lambda,
                };
                true
            }
        }
    }

    fn change(&mut self, _: Self::Properties) -> ShouldRender {
        false
    }

    fn view(&self) -> Html {
        let syntax = self.persistent_data.borrow().syntax;
        html! {
            <>
                <div id="defsColumn">
                    { self.persistent_data.borrow().defs.accessible_defs().iter().map(|def| html! {
                        <div class="monospace box shrinkBorder">{def.display(syntax)}</div>
                    }).collect::<Html>() }
                </div>
                <div id="replColumn">
                    { self.eval_results.iter().enumerate()
                        .map(|(i, eval_result)| eval_result.to_html(i, &self.link))
                        .collect::<Html>() }
                    <div class="row">
                        <input id="statementInput" placeholder="(λa b. a) x y" class="monospace"
                            type="text" autofocus=true value=self.cur_statement.clone()
                            oninput=self.link.callback(|input: InputData| Msg::ModifyCurStatement(input.value))
                            onkeydown=self.link.callback(Msg::KeyDown) />
                        <button class="uiButton" onclick=self.link.callback(|_| Msg::ShowHelp)>{"Help"}</button>
                        <button class="uiButton" onclick=self.link.callback(|_| Msg::SwitchSyntax)>{"Toggle syntax"}</button>
                    </div>
                </div>
            </>
        }
    }

    fn rendered(&mut self, first_render: bool) {
        if first_render {
            document()
                .get_element_by_id("statementInput")
                .unwrap()
                .dyn_into::<HtmlElement>()
                .unwrap()
                .focus()
                .unwrap();
        }
        if self.scroll_defs {
            let defs_column = document().get_element_by_id("defsColumn").unwrap();
            defs_column.scroll_to_with_x_and_y(0.0, defs_column.scroll_height() as f64);
            self.scroll_defs = false;
        }
        if self.scroll_repl {
            let repl_column = document().get_element_by_id("replColumn").unwrap();
            repl_column.scroll_to_with_x_and_y(0.0, repl_column.scroll_height() as f64);
            self.scroll_repl = false;
        }
        if let Some((start, end)) = self.selection_range.take() {
            document()
                .get_element_by_id("statementInput")
                .unwrap()
                .dyn_into::<HtmlInputElement>()
                .unwrap()
                .set_selection_range(start, end)
                .unwrap();
        }
    }
}

#[wasm_bindgen(start)]
pub fn main() {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));

    yew::start_app::<LambdaCalculus>();
}
