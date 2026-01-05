# Frontend package derivation
# Packages pre-built WASM frontend files
#
# Note: Frontend must be built manually before running nix build:
#   ./deploy_frontend.sh
#
# This copies the pre-built files from static/ directory.
# Building WASM in Nix sandbox is complex due to Hackage dependency resolution.
{ pkgs, src }:

pkgs.stdenv.mkDerivation {
  pname = "competences-frontend";
  version = "0.2.1";

  inherit src;

  dontBuild = true;

  installPhase = ''
    mkdir -p $out

    # Copy pre-built WASM files from static directory
    if [ -f static/app.wasm ]; then
      cp static/app.wasm $out/
    else
      echo "ERROR: static/app.wasm not found. Run ./deploy_frontend.sh first."
      exit 1
    fi

    if [ -f static/ghc_wasm_jsffi.js ]; then
      cp static/ghc_wasm_jsffi.js $out/
    else
      echo "ERROR: static/ghc_wasm_jsffi.js not found. Run ./deploy_frontend.sh first."
      exit 1
    fi

    if [ -f static/index.js ]; then
      cp static/index.js $out/
    fi

    if [ -f static/output.css ]; then
      cp static/output.css $out/
    fi

    if [ -f static/basecoat.cdn.min.css ]; then
      cp static/basecoat.cdn.min.css $out/
    fi

    # Copy WASI shim files
    if [ -d static/wasi ]; then
      cp -r static/wasi $out/
    else
      echo "ERROR: static/wasi directory not found. Run the WASI download script first."
      exit 1
    fi

    echo "Frontend static files packaged to $out"
    ls -lah $out/
  '';
}
