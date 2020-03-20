{ nixpkgs ? <nixpkgs> }:

let
  spec = builtins.fromJSON (builtins.readFile ./haskell-nix-src.json);
  haskell-nix-src = (import nixpkgs { }).fetchgit {
    name = "haskell-lib";
    inherit (spec) url rev sha256 fetchSubmodules;
  };
  pkgs = import nixpkgs (import haskell-nix-src { }).nixpkgsArgs;
  modules = haskellPackages: [
    { reinstallableLibGhc = true; }
    { packages.ghc.patches = [ ./ghc.patch ]; }
    { packages.terminal-size.patches = [ ./terminal-size.patch ]; }
    {
      packages.tenpureto.components.tests.tenpureto-test.build-tools =
        [ haskellPackages.tasty-discover ];
    }
    {
      packages.tenpureto.components.tests.tenpureto-test.testWrapper =
        [ "echo" ];
    }
  ];
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    src = ./.;
    name = "tenpureto";
  };
in {
  default = with pkgs;
    haskell-nix.stackProject {
      inherit src;
      modules = modules haskell-nix.haskellPackages;
    };
  static = with pkgs.pkgsCross.musl64;
    let
      libffi-static = libffi.overrideAttrs (oldAttrs: {
        dontDisableStatic = true;
        configureFlags = (oldAttrs.configureFlags or [ ])
          ++ [ "--enable-static" "--disable-shared" ];
      });
    in haskell-nix.stackProject {
      inherit src;
      modules = (modules haskell-nix.haskellPackages) ++ [
        { doHaddock = false; }
        {
          ghc.package =
            buildPackages.pkgs.haskell-nix.compiler.ghc883.override {
              enableIntegerSimple = true;
              enableShared = true;
            };
        }
        { packages.ghc.flags.terminfo = false; }
        { packages.bytestring.flags.integer-simple = true; }
        { packages.text.flags.integer-simple = true; }
        { packages.scientific.flags.integer-simple = true; }
        { packages.integer-logarithms.flags.integer-gmp = false; }
        { packages.cryptonite.flags.integer-gmp = false; }
        { packages.hashable.flags.integer-gmp = false; }
        {
          packages.polysemy.build-tools =
            [ haskell-nix.haskellPackages.cabal-doctest ];
        }
        {
          packages.tenpureto.components.exes.tenpureto.configureFlags = [
            "--disable-executable-dynamic"
            "--disable-shared"
            "--ghc-option=-optl=-pthread"
            "--ghc-option=-optl=-static"
            "--ghc-option=-optl=-L${libffi-static}/lib"
          ];
        }
      ];
    };
  # debug
  inherit pkgs;
}
