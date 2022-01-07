{
  description = "Dev Setup";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = {nixpkgs, flake-utils, ...}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in rec {
        packages = rec {
          dev-env = pkgs.buildEnv {
            name = "jsonpath-hs-dev";
            paths = [
              # Tools
              pkgs.haskell.compiler.ghc901
              pkgs.haskellPackages.cabal-install
              (pkgs.haskell-language-server.override {supportedGhcVersions = ["901"];})
              pkgs.haskellPackages.implicit-hie
              pkgs.cabal2nix

              # For cabal
              pkgs.pkgconfig
              pkgs.binutils

              # For CI
              pkgs.jq
              pkgs.dhall
              pkgs.dhall-json
              pkgs.fly
            ];
          };
          jsonpath-ghc901 = pkgs.haskell.packages.ghc901.callPackage ./default.nix {};
          jsonpath-ghc8107 = pkgs.haskell.packages.ghc8107.callPackage ./default.nix {};
          jsonpath-ghc884 = pkgs.haskell.packages.ghc884.callPackage ./default.nix {};
        };
        defaultPackage = packages.dev-env;
    });
}
