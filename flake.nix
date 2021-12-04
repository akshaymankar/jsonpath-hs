{
  description = "Dev Setup";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = {nixpkgs, flake-utils, ...}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in rec {
        packages.dev-env = pkgs.buildEnv {
          name = "jsonpath-hs-dev";
          paths = [
            # Tools
            pkgs.haskell.compiler.ghc901
            pkgs.haskellPackages.cabal-install
            pkgs.gnumake
            (pkgs.haskell-language-server.override {supportedGhcVersions = ["901"];})
            pkgs.haskellPackages.implicit-hie

            # For cabal
            pkgs.pkgconfig
            pkgs.binutils
          ];
        };
        defaultPackage = packages.dev-env;
    });
}
