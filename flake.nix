{
  description = "Dev Setup";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = {nixpkgs, flake-utils, ...}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in rec {
        packages = {
          dev-env = pkgs.buildEnv {
            name = "jsonpath-hs-dev";
            paths = [
              # Tools
              pkgs.haskell.compiler.ghc901
              pkgs.haskellPackages.cabal-install
              (pkgs.haskell-language-server.override {supportedGhcVersions = ["901"];})
              pkgs.haskellPackages.implicit-hie

              # For cabal
              pkgs.pkgconfig
              pkgs.binutils
            ];
          };
          jsonpath-ghc901 = pkgs.haskell.packages.ghc901.callPackage ./default.nix {};
          jsonpath-ghc8107 = pkgs.haskell.packages.ghc8107.callPackage ./default.nix {};
          jsonpath-ghc884 = pkgs.haskell.packages.ghc884.callPackage ./default.nix {};
          ci-env-ghc901 = pkgs.buildEnv {
            name = "jsonpath-hs-ci";
            paths = [
              pkgs.haskell.compiler.ghc901
              pkgs.haskellPackages.cabal-install
              pkgs.wget
              pkgs.pkgconfig
              pkgs.binutils
            ];
          };
          ci-env-ghc8107 = pkgs.buildEnv {
            name = "jsonpath-hs-ci";
            paths = [
              pkgs.haskell.compiler.ghc8107
              pkgs.haskellPackages.cabal-install
              pkgs.wget
              pkgs.pkgconfig
              pkgs.binutils
            ];
          };
          ci-env-ghc884 = pkgs.buildEnv {
            name = "jsonpath-hs-ci";
            paths = [
              pkgs.haskell.compiler.ghc884
              pkgs.haskellPackages.cabal-install
              pkgs.wget
              pkgs.pkgconfig
              pkgs.binutils
            ];
          };
        };
        defaultPackage = packages.dev-env;
    });
}
