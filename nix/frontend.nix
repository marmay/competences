# Frontend package derivation: produces the assembled static/ tree for
# deployment. Mirrors deploy_frontend.sh's logic but as a hermetic Nix
# derivation. The dev shell's deploy_frontend.sh references the same
# npm-deps derivation (via `competences-frontend.passthru.npmDeps`), so
# both paths share a single source for MathJax / basecoat-css.
{ pkgs, src, hixProject }:

let
  npmDeps = import ./npm-deps.nix { inherit pkgs src; };

  wasmExe = hixProject.projectCross.wasi32.hsPkgs
    .competences-frontend.components.exes.competences-frontend;

  # The cross GHC's shell environment. We need its lib/ for
  # post-link.mjs (the JS FFI shim generator).
  wasmGhc = hixProject.projectCross.wasi32.shell.ghc;
in

pkgs.stdenv.mkDerivation rec {
  pname = "competences-frontend";
  version = "1.6.1";

  inherit src;

  nativeBuildInputs = with pkgs; [
    binaryen      # wasm-opt
    wasm-tools
    esbuild
    tailwindcss_4
    nodejs        # for post-link.mjs
  ];

  dontUnpack = false;

  buildPhase = ''
    runHook preBuild

    # 1. Run post-link.mjs to extract the JS FFI shim.
    node ${wasmGhc}/lib/post-link.mjs \
      --input ${wasmExe}/bin/competences-frontend.wasm \
      --output ghc_wasm_jsffi.js

    # 2. Optimise + strip the .wasm.
    cp ${wasmExe}/bin/competences-frontend.wasm app.wasm
    chmod +w app.wasm
    wasm-opt -O4 app.wasm -o app.wasm
    wasm-tools strip -o app.wasm app.wasm

    # 3. Bundle the JS loader. esbuild needs ./wasi/index.js relative to
    # frontend/static-src/index.js — keep them together in a temp dir.
    mkdir -p bundle-tmp
    cp -r frontend/static-src/wasi bundle-tmp/
    cp frontend/static-src/index.js bundle-tmp/
    esbuild bundle-tmp/index.js \
      --bundle --format=esm --outfile=index.js --minify

    # 4. Stage the Nix-built node_modules. tailwindcss resolves
    # `@import "basecoat-css"` against ./node_modules, MathJax assets
    # live there too. Same path the dev shell exposes, so production
    # and dev iterate against an identical tree.
    ln -sfn ${npmDeps}/node_modules ./node_modules

    # 5. Compile Tailwind.
    tailwindcss \
      -i frontend/static-src/input.css \
      -o output.css --minify

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out

    # Build outputs
    cp app.wasm $out/
    cp ghc_wasm_jsffi.js $out/
    cp index.js $out/
    cp output.css $out/

    # Vendored static assets (kept in the main repo under
    # frontend/static-src/ — see commit dropping the static submodule).
    cp -r frontend/static-src/fonts $out/
    cp -r frontend/static-src/wasi $out/

    # MathJax bundle (from the Nix-built node_modules symlink).
    cp node_modules/mathjax/tex-svg.js $out/mathjax-tex-svg.js
    mkdir -p $out/sre
    cp node_modules/mathjax/sre/speech-worker.js $out/sre/
    cp -rL node_modules/@mathjax/mathjax-newcm-font $out/mathjax-newcm-font

    # Sanity check
    test -f $out/app.wasm
    test -f $out/index.js
    test -f $out/ghc_wasm_jsffi.js
    test -f $out/output.css

    runHook postInstall
  '';

  passthru = { inherit npmDeps; };
}
