#!/bin/sh

set -e

CABAL=$(which wasm32-wasi-cabal)
GHC=$(which wasm32-wasi-ghc)

$CABAL build exe:competences-frontend
$($GHC --print-libdir)/post-link.mjs --input $($CABAL list-bin exe:competences-frontend --allow-newer) --output static/ghc_wasm_jsffi.js
cp $($CABAL list-bin exe:competences-frontend --allow-newer) static/app.wasm 
