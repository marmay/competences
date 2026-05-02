#!/bin/sh
# Build the WASM frontend and assemble the static/ tree for deployment.
#
# Run from inside the project's dev shell (it provides
# wasm32-unknown-wasi-cabal, binaryen, wasm-tools, esbuild, tailwindcss):
#
#   nix develop --command ./deploy_frontend.sh
#
# The .wasm is built incrementally via the haskell.nix-provided
# wasm32-unknown-wasi-cabal — module-level, so only modules you touched
# rebuild between runs. For a hermetic from-scratch build, use
# `nix build .#wasm32-unknown-wasi:competences-frontend:exe:competences-frontend`
# instead.

set -x
set -e

# 1. Build the .wasm incrementally.
wasm32-unknown-wasi-cabal build exe:competences-frontend
WASM_BIN=$(wasm32-unknown-wasi-cabal list-bin exe:competences-frontend)

# 2. Generate the JS FFI shim using post-link.mjs from the cross GHC.
GHC_LIB=$(wasm32-unknown-wasi-ghc --print-libdir)
"$GHC_LIB/post-link.mjs" --input "$WASM_BIN" --output static/ghc_wasm_jsffi.js

# 3. Copy + optimise + strip the .wasm into static/.
cp "$WASM_BIN" static/app.wasm
chmod +w static/app.wasm
wasm-opt -O4 static/app.wasm -o static/app.wasm
wasm-tools strip -o static/app.wasm static/app.wasm

# 4. Bundle the JS loader.
echo "Bundling index.js with esbuild..."
esbuild frontend/static-src/index.js --bundle --format=esm --outfile=static/index.js --minify

# 5. MathJax assets (still pulled from node_modules — pinned via package.json).
echo "Copying MathJax bundle..."
cp node_modules/mathjax/tex-svg.js static/mathjax-tex-svg.js

echo "Copying MathJax SRE speech worker (for future a11y use)..."
mkdir -p static/sre
cp node_modules/mathjax/sre/speech-worker.js static/sre/speech-worker.js

echo "Copying MathJax font data..."
rm -rf static/mathjax-newcm-font
cp -r node_modules/@mathjax/mathjax-newcm-font static/mathjax-newcm-font

# 6. Compile Tailwind.
echo "Building Tailwind CSS..."
tailwindcss -i ./frontend/static-src/input.css -o ./static/output.css --minify
