{
  description = "Competences tracking application with Haskell backend and WASM frontend";

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.competences-blobs = {
    url = "github:marmay/competences-blobs";
    flake = false;
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, competences-blobs }:
    let
      supportedSystems = [
        "x86_64-linux"
      ];
    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [ haskellNix.overlay
          (final: _prev: {
            hixProject = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc9141";

              # Per-platform module overrides applied only when building for
              # wasm32. We declare which packages are WASM-buildable (only
              # competences-{common,markdown,frontend}) and inject the same
              # flags the legacy cabal.project.wasm used. The remaining
              # packages depend on POSIX libraries (warp, sqlite, libsodium,
              # …) without wasi32 configurations and are simply marked
              # non-buildable on this platform.
              modules = [({ lib, pkgs, ... }:
                lib.mkIf pkgs.stdenv.hostPlatform.isWasm {
                  packages.competences-common.flags.aeson = false;
                  packages.competences-frontend.flags.wasm = true;
                  packages.competences-backend.package.buildable = lib.mkForce false;
                  packages.competences-csvconvert.package.buildable = lib.mkForce false;
                  packages.competences-housecup.package.buildable = lib.mkForce false;
                })];

              # Make wasm32-unknown-wasi-{cabal,ghc} available in `nix develop`
              # so the WASM frontend can be iterated incrementally with cabal,
              # without going through hermetic `nix build`s every time.
              shell.crossPlatforms = p: [ p.wasi32 ];

              shell.tools.cabal = "latest";
              # hlint and haskell-language-server temporarily dropped — neither
              # has a GHC 9.14 / base 4.22 compatible release yet. Re-add once
              # upstream catches up.
              # shell.tools.hlint = "latest";
              # shell.tools.haskell-language-server = "latest";

              # Native dev plus the post-processing pipeline the deploy
              # script needs (binaryen for wasm-opt, wasm-tools, esbuild,
              # tailwindcss). The wasm32-unknown-wasi-cabal wrapper from
              # haskell.nix is also on PATH but not currently usable for
              # direct iterative builds — it picks up the native libffi
              # rather than the wasi32 one. The deploy script therefore
              # invokes `nix build` for the .wasm artifact and only uses
              # the post-processing tools from the shell. Module-level
              # incremental WASM dev is a follow-up item.
              shell.buildInputs = with final; [
                ghcid ghciwatch nginx postgresql
                binaryen wasm-tools esbuild tailwindcss_4
                gnumake http-server
              ];
            };
          })
          # Overlay to add competences packages to pkgs (for NixOS module)
          (final: _prev: {
            competences-backend = import ./nix/backend.nix {
              inherit (final) hixProject;
            };
            competences-frontend = import ./nix/frontend.nix {
              pkgs = final;
              blobs = competences-blobs;
              src = ./.;
            };
            competences-housecup = import ./nix/housecup.nix {
              inherit (final) hixProject;
            };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.hixProject.flake {
          crossPlatforms = p: [ p.wasi32 ];
        };

        # Get packages from pkgs (which now has our overlay applied)
        backend = pkgs.competences-backend;
        frontend = pkgs.competences-frontend;
        housecup = pkgs.competences-housecup;
      in flake // {
        legacyPackages = pkgs;

        # Add explicit package outputs for deployment
        packages = flake.packages // {
          competences-backend = backend;
          competences-frontend = frontend;
          competences-housecup = housecup;

          # Combined package for convenience
          default = pkgs.symlinkJoin {
            name = "competences";
            paths = [ backend frontend ];
          };
        };
      }) // {
      # NixOS module (system-agnostic)
      nixosModules.competences = import ./nix/module.nix;
    };

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}
