#!/bin/sh

set -e

echo "If you are missing any of the tools, please run:"
echo "  nix develop .#wasmShell.x86_64-linux"
CABAL=$(which wasm32-wasi-cabal)
GHC=$(which wasm32-wasi-ghc)
WASM_OPT=$(which wasm-opt)
WASM_TOOLS=$(which wasm-tools)

$CABAL update
$CABAL build -f "+wasm" exe:competences-frontend
$($GHC --print-libdir)/post-link.mjs --input $($CABAL list-bin exe:competences-frontend --allow-newer) --output static/ghc_wasm_jsffi.js
cp $($CABAL list-bin exe:competences-frontend --allow-newer) static/app.wasm 
$WASM_OPT -all -O2 static/app.wasm -o static/app.wasm
$WASM_TOOLS strip -o static/app.wasm static/app.wasm
