{
  description = "Dev Setup";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = {nixpkgs, flake-utils, ...}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        ghcOverrides = hself: hsuper: rec {
          jsonpath = hsuper.callPackage ./default.nix {};
        };
        ghc902Pkgs = pkgs.haskell.packages.ghc902.override {
          overrides = ghcOverrides;
        };
        ghc8107Pkgs = pkgs.haskell.packages.ghc8107.override {
          overrides = ghcOverrides;
        };
        ghc884Pkgs = pkgs.haskell.packages.ghc884.override {
          overrides = ghcOverrides;
        };
      in rec {
        packages = rec {
          dev-env = ghc902Pkgs.shellFor {
            packages = p: [p.jsonpath];
            buildInputs = [
              pkgs.haskellPackages.cabal-install
              (pkgs.haskell-language-server.override {supportedGhcVersions = ["902"];})
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
          jsonpath-ghc902 = ghc902Pkgs.jsonpath;
          jsonpath-ghc8107 = ghc8107Pkgs.jsonpath;
          jsonpath-ghc884 = ghc884Pkgs.jsonpath;
        };
        defaultPackage = packages.dev-env;
    });
}
