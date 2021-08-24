Lambda Calculus is a simple model of computation, with the only data type being functions that take one argument and return one result. Despite its simplicity, it's Turing-complete. For more information about it, see the [Wikipedia page](https://en.wikipedia.org/wiki/Lambda_calculus), the [Wikibooks page](https://en.wikibooks.org/wiki/Programming_Languages/Semantics_Specification#The_Built-in_Operations_of_Lambda_Calculus), or many other sources.

This is a small project to experiment with lambda calculus. It's not intended to be useful in production or be feature-complete. See the [online version here](https://nstoddard.github.io/lambda-calculus/index.html).

This app also has a command-line version; to run it, clone this repo and run `cargo run`. To compile and run the online version yourself, install [`wasm-bindgen`](https://github.com/rustwasm/wasm-bindgen), run `./build-debug.sh`, and use a web server to serve the `static/` directory. Release builds (`./build-release.sh`) also require [`wasm-opt`](https://github.com/WebAssembly/binaryen) and [`terser`](https://github.com/terser-js/terser).

This was tested in Firefox and Chrome, on Linux. It should also work on other operating systems, and may or may not work in other browsers. If there's any problems, please file an issue.
