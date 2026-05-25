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

# Use a separate dist tree from native `cabal build`. The inplace package db
# under `dist-newstyle/packagedb/` is not per-target, so a native build of
# `competences-common` (with the +aeson flag) would clobber the WASM
# registration (built with -aeson) and break the next WASM link.
WASM_BUILDDIR=dist-newstyle-wasm

mkdir -p static

# 1. Build the .wasm incrementally.
wasm32-unknown-wasi-cabal --builddir="$WASM_BUILDDIR" build exe:competences-frontend
WASM_BIN=$(wasm32-unknown-wasi-cabal --builddir="$WASM_BUILDDIR" list-bin exe:competences-frontend)

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

# 5. Vendored static assets (kept in-tree under frontend/static-src/).
#    Use `install -m 644` instead of `cp` — sources may live in the Nix
#    store and be read-only; `cp` preserves that mode, so a second run
#    can't overwrite the previous outputs. `install` sets a writable
#    destination mode in one syscall.
echo "Copying vendored fonts and WASI shim..."
mkdir -p static/fonts static/wasi
install -m 644 -t static/fonts/ frontend/static-src/fonts/*.woff2
install -m 644 -t static/wasi/ frontend/static-src/wasi/*.js

# 6. MathJax assets (still pulled from node_modules — pinned via package.json).
echo "Copying MathJax bundle..."
install -m 644 node_modules/mathjax/tex-svg.js static/mathjax-tex-svg.js

echo "Copying MathJax SRE speech worker (for future a11y use)..."
mkdir -p static/sre
install -m 644 node_modules/mathjax/sre/speech-worker.js static/sre/speech-worker.js

echo "Copying MathJax font data..."
rm -rf static/mathjax-newcm-font
# --no-preserve=mode applies umask (typically 755 dirs / 644 files) instead
# of inheriting the Nix store's read-only mode bits — same reasoning as the
# install -m 644 calls above, but for a recursive directory tree.
cp -r --no-preserve=mode node_modules/@mathjax/mathjax-newcm-font static/mathjax-newcm-font

# 7. Compile Tailwind.
echo "Building Tailwind CSS..."
tailwindcss -i ./frontend/static-src/input.css -o ./static/output.css --minify
