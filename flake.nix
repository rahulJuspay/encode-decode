{
  description = "Flake system for encode-decode";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/747927516efcb5e31ba03b7ff32f61f6d47e7d87";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    #haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, flake-compat, ... }:
    let
      ghcFor = pkgs: pkgs.haskell.packages.ghc94;
      enableHsProfiling = _hfinal: hprev: {
                  mkDerivation = args: hprev.mkDerivation (args // {
                    enableLibraryProfiling = true;
                    enableExecutableProfiling = true;
                  });
                };
      composeExtensions = pkgs: hsOverrides:
        pkgs.lib.composeManyExtensions [
          # enableHsProfiling
          hsOverrides
          # enableHsProfiling
        ];
    in
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs =
          import nixpkgs { inherit system; config = { allowUnfree = true; };};
        hp = 
          (pkgs.haskell.packages.ghc94.extend (self: super: {
            mkDerivation = args:
              super.mkDerivation (args // { enableLibraryProfiling = true; enableExecutableProfiling = true; }); })).extend(composeExtensions pkgs (overrides pkgs));
        overrides = pkgs: import ./overlay.nix { inherit pkgs; };
        encode-decode = hp.encode-decode;
        shellDeps = hp:
          with hp;
          [ cabal-install
            # ghcid
            hlint
            pkgs.nixpkgs-fmt
            hp2html
            pkgs.cassandra_4
            pkgs.kcat
            pkgs.just
            hp2pretty ];
      in
      {
        packages = { inherit encode-decode; };
        legacyPackages = import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
          crossOverlays = [ self.overlay ];
        };

        defaultPackage = self.packages.${system}.encode-decode;

        devShells.default =
          hp.shellFor {
            packages = p: [ p.encode-decode ];
            withHoogle = true;
            buildInputs = shellDeps hp;
            genericBuilderArgsModifier = args: args // { enableLibraryProfiling = true; enableExecutableProfiling = true; };
        };
      });
}