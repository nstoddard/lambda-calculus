use yew::*;

#[rustfmt::skip]
pub fn help_html() -> Html {
    html! {
<div class="box info">
// These multiline string literals have to be aligned to the left or each line
// ends up with extra spaces at the start.
// TODO: find a better way to produce this help text; maybe generate it from markdown
{"Lambda Calculus interpreter, by Nathan Stoddard
    
Lambda Calculus is a simple model of computation, with the only data type being functions that take one argument and return one result. Despite its simplicity, it's Turing-complete. For more information about it, see the "}
<a href="https://en.wikipedia.org/wiki/Lambda_calculus">{"Wikipedia page"}</a>
{", the "}
<a href="https://en.wikibooks.org/wiki/Programming_Languages/Semantics_Specification#The_Built-in_Operations_of_Lambda_Calculus">
    {"Wikibooks page"}
</a>
{", or many other sources.

This is a small project to experiment with lambda calculus. It's not intended to be useful in production or be feature-complete. It supports two syntaxes for functions:
    Standard lambda calculus syntax: "}<span class="monospace">{"λa. a"}</span>{"
    Arrow syntax: "}<span class="monospace">{"a -> a"}</span>{"
    You can switch between these two syntaxes at any time. Backslashes ("}<span class="monospace">{'\\'}</span>{") may be used in place of lambda characters.

Syntax:
    Functions: "} <span class="monospace">{ "λa. a" }</span>
        {" or "} <span class="monospace">{ "a -> a" }</span>
    {"\n    Function application: "} <span class="monospace">{ "(λa b. a) x y" }</span>
        {" or "} <span class="monospace">{"(a -> b -> a) x y"}</span>
    {"\n    Definitions: "} <span class="monospace">{ "id = λa. a" }</span>
        {" or "} <span class="monospace">{"id = a -> a"}</span>
{"

Names can either be alphanumeric (and unlike in most languages, can start with a digit), or contain only symbols (most ASCII characters are allowed).
Definitions are substituted into expressions before evaluation, so they can't be used for recursion.
The output of evaluating an expression is typically displayed twice: a minimal form in terms of definitions where possible, followed by the full expression (if different).

Commands:
    help: display this help info
    reset: remove all definitions
    undefine foo: remove the definition for 'foo'
    defs: display everything that has been defined (only needed in the desktop version)

This was tested in Firefox and Chrome, on Linux. It should also work on other operating systems, and may or may not work in other browsers. If there's any problems, please file an issue at " }
<a href="https://github.com/nstoddard/lambda-calculus/">{"the GitHub repository"}</a>
{"."}
</div>
    }
}
