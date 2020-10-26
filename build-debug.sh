#!/usr/bin/env bash

trap 'exit' ERR

cargo build --lib --target wasm32-unknown-unknown
wasm-bindgen target/wasm32-unknown-unknown/debug/lambda_calculus_wasm.wasm --out-dir static/generated --no-modules --no-typescript --debug
