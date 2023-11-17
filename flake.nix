{
  description = "Flake system for encode-decode";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    nixpkgs-140774-workaround.url = "github:srid/nixpkgs-140774-workaround";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
  };
  outputs = inputs@{ self, nixpkgs, flake-compat, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } ({ withSystem, ... }: {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = { self', inputs', pkgs, system, ... }: {
        legacyPackages = pkgs;
        haskellProjects.default = {
          imports = [
            inputs.nixpkgs-140774-workaround.haskellFlakeProjectModules.default
            self.haskellFlakeProjectModules.input
          ];
          packages.encode-decode.root = ./.;

          devShell = {
            enable = true;
            hlsCheck.enable = true;
            tools = hp: {
              inherit (pkgs)
                nixpkgs-fmt cassandra_4 kcat just;
              inherit (hp)
                cabal-install ghcid fourmolu hp2html hp2pretty;
              # Dependency for cqlsh
              inherit (pkgs.python3Packages)
                greenlet;
              # cachix = inputs'.cachix.packages.default;
            };
          };
        };
        packages.default = self'.packages.encode-decode;
      };
      flake.haskellFlakeProjectModules = rec {
        input = { pkgs, ... }: {
          overrides = import ./overlay.nix { inherit pkgs; };
        };
        output = { pkgs, lib, ... }: withSystem pkgs.system (ctx@{ config, ... }: {
          imports = [
            input
          ];
          source-overrides =
            lib.mapAttrs (name: ks: ks.root)
              config.haskellProjects.default.packages;
        });
      };
    });
}
