# Hermetic node_modules built from package.json + package-lock.json.
# Used both by nix/frontend.nix (for the production static-tree derivation)
# and by the dev shell (so deploy_frontend.sh / tailwindcss don't rely on
# a local `npm install`). One source of truth.
{ pkgs, src }:

pkgs.buildNpmPackage {
  pname = "competences-npm-deps";
  version = "1.0.0";
  src = pkgs.lib.fileset.toSource {
    root = src;
    fileset = pkgs.lib.fileset.unions [
      (src + "/package.json")
      (src + "/package-lock.json")
    ];
  };

  # Computed from package-lock.json. Update via `lib.fakeHash` trick
  # whenever the lockfile changes.
  npmDepsHash = "sha256-dI8DcBG07IS3vL2EGeYaP1KYxaxxaSAkdug7SA3chR8=";

  # We don't need to run an npm build script — just want node_modules.
  dontNpmBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out
    cp -r node_modules $out/
    runHook postInstall
  '';
}
