{
  description = "Flake system for encode-decode";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    nixpkgs-140774-workaround.url = "github:srid/nixpkgs-140774-workaround";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
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
