# Frontend package derivation
# Packages pre-built WASM frontend files from the competences-blobs repo
# plus index.js from the main repo's frontend/static-src/.
#
# Note: Frontend must be built manually before creating a release:
#   ./deploy_frontend.sh
#   cd static && git add -A && git commit -m "Release X.Y.Z" && git push && cd ..
#   nix flake lock --update-input competences-blobs
{ pkgs, blobs, src }:

pkgs.stdenv.mkDerivation {
  pname = "competences-frontend";
  version = "0.10.0";

  dontUnpack = true;
  dontBuild = true;

  installPhase = ''
    mkdir -p $out

    # Copy blob files (from competences-blobs repo)
    cp -r ${blobs}/. $out/

    # Copy source files from main repo
    cp ${src}/frontend/static-src/index.js $out/

    # Verify critical files
    test -f $out/app.wasm || (echo "ERROR: app.wasm not found" && exit 1)
    test -f $out/index.js || (echo "ERROR: index.js not found" && exit 1)
    test -d $out/wasi || (echo "ERROR: wasi/ not found" && exit 1)

    echo "Frontend static files packaged to $out"
    ls -lah $out/
  '';
}
