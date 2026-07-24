{
  description = "Competences tracking application with Haskell backend and WASM frontend";

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
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
                  packages.marmay-auth.package.buildable = lib.mkForce false;
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
              shell.tools.haskell-language-server = {
                modules = [{
                  doCheck = false;
                }];
                cabalProjectLocal = ''
                  package haskell-language-server
                    flags: -ghcide-bench
                  allow-newer: *:base, *:containers, *:template-haskell, *:ghc, *:time
                '';
	      };

              # Native dev tools and the WASM post-processing pipeline.
              # These are *build-platform* binaries (they produce or run on
              # x86_64-linux), so they go into nativeBuildInputs. Putting
              # them into buildInputs would treat them as cross-targeted
              # deps and leak their .so/.a paths into NIX_LDFLAGS_FOR_TARGET,
              # which then poisons the wasi32 link search path and points
              # `-lffi` at the native libffi.
              shell.nativeBuildInputs = with final; [
                ghcid ghciwatch nginx postgresql
                binaryen wasm-tools esbuild tailwindcss_4
                gnumake http-server
              ];

              # Bypass nixpkgs' cross-shell aggregation, which dumps the
              # union of native libs (libffi-3.5.2, gmp, ncurses, …) and
              # wasi32 libs (libffi-wasm, libcxx-static-wasm32-unknown-wasi,
              # …) into both NIX_LDFLAGS and NIX_LDFLAGS_FOR_TARGET. Each
              # cc-wrapper's add-flags.sh hook would then mangle them into
              # the per-target var, leaking wasi paths to ld.bfd and native
              # paths to wasm-ld. We zero the shared inputs and set the
              # per-target vars directly, then stamp the FLAGS_SET marker
              # so the wrappers don't try to re-append on first invocation.
              shell.shellHook = ''
                # Force node_modules to point at the Nix-built deps so
                # deploy_frontend.sh / tailwindcss / esbuild see the same
                # tree the production derivation does. Always overwrite —
                # no `npm install` should ever be the source of truth.
                rm -rf node_modules
                ln -snf ${final.competences-frontend.passthru.npmDeps}/node_modules node_modules

                _split_keep_wasm() {
                  local out=""
                  for tok in $1; do
                    case "$tok" in
                      -L*wasm*|-L*wasi*) out="$out $tok" ;;
                      -L*) ;;
                      *) out="$out $tok" ;;
                    esac
                  done
                  printf '%s' "$out"
                }
                _split_drop_wasm() {
                  local out=""
                  for tok in $1; do
                    case "$tok" in
                      -L*wasm*|-L*wasi*) ;;
                      *) out="$out $tok" ;;
                    esac
                  done
                  printf '%s' "$out"
                }
                # NIX_LDFLAGS feeds the native cc-wrapper (HOST role), keep
                # native libs only. NIX_LDFLAGS_FOR_TARGET feeds *both* the
                # native cc-wrapper (its own TARGET role for x86_64) and
                # wasm32-unknown-wasi-cc (TARGET role for wasi32). Empty it
                # and instead pre-populate NIX_LDFLAGS_wasm32_unknown_wasi
                # so add-flags.sh appends nothing extra to it.
                _all_lf="''${NIX_LDFLAGS:-} ''${NIX_LDFLAGS_FOR_TARGET:-}"
                export NIX_LDFLAGS="$(_split_drop_wasm "$_all_lf")"
                export NIX_LDFLAGS_FOR_TARGET=""
                export NIX_LDFLAGS_wasm32_unknown_wasi="$(_split_keep_wasm "$_all_lf") ''${NIX_LDFLAGS_wasm32_unknown_wasi:-}"
                unset _all_lf
              '';
            };
          })
          # Overlay to add competences packages to pkgs (for NixOS module)
          (final: _prev: {
            competences-backend = import ./nix/backend.nix {
              inherit (final) hixProject;
            };
            competences-frontend = import ./nix/frontend.nix {
              pkgs = final;
              hixProject = final.hixProject;
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
