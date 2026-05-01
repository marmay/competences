{pkgs, ...}: {
  name = "competences";
  compiler-nix-name = "ghc9141"; # Version of GHC to use

  # WASM cross outputs are exposed via the flake call (see flake.nix).
  # Keep the `crossPlatforms` setting *out* of this top-level project config
  # — adding it here pollutes the dev shell with cross-compiled deps and
  # breaks `nix develop` / `cabal build all` for native targets.

  # Per-platform flag overrides for the wasi32 cross build. These mirror
  # the legacy cabal.project.wasm flags so a single cabal.project file
  # serves both the native and WASM builds.
  modules = [({ lib, pkgs, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isWasm {
    packages.competences-common.flags.aeson = false;
    packages.competences-frontend.flags.wasm = true;
  })];

  # Tools to include in the development shell
  shell.buildInputs = with pkgs; [ ghcid ghciwatch nginx postgresql ];

  shell.tools.cabal = "latest";
  # hlint and haskell-language-server temporarily dropped — neither has
  # a GHC 9.14 / base 4.22 compatible release yet (hlint pins ghc-lib-parser
  # <9.13; HLS pins boring <4.22). Re-add once upstream catches up.
  # shell.tools.hlint = "latest";
  # shell.tools.haskell-language-server = "latest";
}
