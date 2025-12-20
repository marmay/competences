{
  description = "Competences tracking application with Haskell backend and WASM frontend";

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";

  outputs = { self, nixpkgs, flake-utils, haskellNix, ghc-wasm-meta }:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [ haskellNix.overlay
          (final: _prev: {
            hixProject =
              final.haskell-nix.hix.project {
                src = ./.;
                # uncomment with your current system for `nix flake show` to work:
                #evalSystem = "x86_64-linux";
              } //
	      { shell.buildInputs = [ ghc-wasm-meta.packages.${system}.all_9_12 ]; };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.hixProject.flake {};
      in flake // {
        legacyPackages = pkgs;

        # Add explicit package outputs for deployment
        packages =
          let
            backend = import ./nix/backend.nix {
              inherit (pkgs) hixProject;
            };
            frontend = import ./nix/frontend.nix {
              inherit pkgs;
              src = ./.;  # Don't use cleanSource - we need the static/ directory
            };
          in flake.packages // {
            competences-backend = backend;
            competences-frontend = frontend;

            # Combined package for convenience
            default = pkgs.symlinkJoin {
              name = "competences";
              paths = [ backend frontend ];
            };
          };
      } // {
        wasmShell = pkgs.mkShell {
          name = "The miso ${system} GHC WASM 9.12.2 shell";
          packages = [
            ghc-wasm-meta.packages.${system}.all_9_12
            pkgs.gnumake
            pkgs.http-server
            pkgs.cabal-install
          ];
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
