#!/usr/bin/env bash

trap 'exit' ERR

export APP=lambda_calculus_wasm

cargo build --lib --release --target wasm32-unknown-unknown
wasm-bindgen target/wasm32-unknown-unknown/release/"$APP".wasm --out-dir static/generated --no-modules --no-typescript
mv static/generated/"$APP"_bg.wasm static/generated/"$APP"_bg_unoptimized.wasm
wasm-opt -O3 -o static/generated/"$APP"_bg.wasm static/generated/"$APP"_bg_unoptimized.wasm
rm static/generated/"$APP"_bg_unoptimized.wasm
terser -m -o static/generated/"$APP".js static/generated/"$APP".js
