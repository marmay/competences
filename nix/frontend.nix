# Frontend package derivation
# Packages pre-built WASM frontend files from the competences-blobs repo
# plus index.js bundled with esbuild from the main repo's frontend/static-src/.
#
# Note: Frontend must be built manually before creating a release:
#   ./deploy_frontend.sh
#   cd static && git add -A && git commit -m "Release X.Y.Z" && git push && cd ..
#   nix flake lock --update-input competences-blobs
{ pkgs, blobs, src }:

pkgs.stdenv.mkDerivation {
  pname = "competences-frontend";
  version = "1.4.0";

  dontUnpack = true;

  nativeBuildInputs = [ pkgs.esbuild ];

  buildPhase = ''
    # Recreate the directory structure so esbuild can resolve the
    # relative import ../../static/wasi/index.js from frontend/static-src/
    mkdir -p frontend/static-src static
    cp ${src}/frontend/static-src/index.js frontend/static-src/
    cp -r ${blobs}/wasi static/

    # Bundle index.js + WASI shim into a single file
    esbuild frontend/static-src/index.js \
      --bundle --format=esm --outfile=index.js --minify
  '';

  installPhase = ''
    mkdir -p $out

    # Copy blob files (from competences-blobs repo)
    cp -r ${blobs}/. $out/

    # Copy bundled index.js
    cp index.js $out/

    # Verify critical files
    test -f $out/app.wasm || (echo "ERROR: app.wasm not found" && exit 1)
    test -f $out/index.js || (echo "ERROR: index.js not found" && exit 1)

    echo "Frontend static files packaged to $out"
    ls -lah $out/
  '';
}
