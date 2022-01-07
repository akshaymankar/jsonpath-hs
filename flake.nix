{
  description = "Dev Setup";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = {nixpkgs, flake-utils, ...}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        aeson2Overrides = hself: hsuper: rec {
          aeson = hsuper.aeson_2_0_2_0;
          OneTuple = hsuper.OneTuple_0_3_1;
          hashable = hsuper.hashable_1_4_0_0;
          quickcheck-instances = hsuper.quickcheck-instances_0_3_26_1;
          text-short = pkgs.haskell.lib.dontCheck hsuper.text-short_0_1_4;
          time-compat = hsuper.time-compat_1_9_6_1;
          semialign = hsuper.semialign_1_2_0_1;
          jsonpath = hsuper.callPackage ./default.nix {};
        };
        ghc901Overrides = hself: hsuper: rec {
          ghc-bignum-orphans = pkgs.haskell.lib.markUnbroken hsuper.ghc-bignum-orphans;
          hashable = pkgs.haskell.lib.overrideCabal hsuper.hashable_1_4_0_0 (args: args // {
            libraryHaskellDepends = args.libraryHaskellDepends ++ [ ghc-bignum-orphans ];
          });
        };
        ghc901Pkgs = pkgs.haskell.packages.ghc901.override {
          overrides = hself: hsuper: (aeson2Overrides hself hsuper // ghc901Overrides hself hsuper);
        };
        ghc8107Pkgs = pkgs.haskell.packages.ghc8107.override {
          overrides = aeson2Overrides;
        };
        ghc884Pkgs = pkgs.haskell.packages.ghc884.override {
          overrides = aeson2Overrides;
        };
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
          jsonpath-ghc901 = ghc901Pkgs.jsonpath;
          jsonpath-ghc8107 = ghc8107Pkgs.jsonpath;
          jsonpath-ghc884 = ghc884Pkgs.jsonpath;
        };
        defaultPackage = packages.dev-env;
    });
}
