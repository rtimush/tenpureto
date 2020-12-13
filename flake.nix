{
  description = "Tenpureto";

  inputs = { haskell.url = "github:input-output-hk/haskell.nix"; };

  outputs = { self, haskell }:
    let

      modules = [
        { reinstallableLibGhc = true; }
        { packages.ghc.patches = [ ./ghc.patch ]; }
        { packages.terminal-size.patches = [ ./terminal-size.patch ]; }
        ({ pkgs, ... }: {
          packages.tenpureto.components.tests.tenpureto-test.build-tools =
            [ pkgs.haskell-nix.haskellPackages.tasty-discover ];
        })
        {
          packages.tenpureto.components.tests.tenpureto-test.testWrapper =
            [ "echo" ];
        }
      ];

      project = system:
        let pkgs = haskell.legacyPackages.${system};
        in pkgs.haskell-nix.stackProject {
          src = pkgs.haskell-nix.haskellLib.cleanGit {
            name = "tenpureto";
            src = ./.;
          };
          modules = modules;
        };

      drv = system: (project system).tenpureto.components.exes.tenpureto;

      staticProject = system:
        let
          pkgs = haskell.legacyPackages.${system};
          pkgsMusl = pkgs.pkgsCross.musl64;
          libffi-static = pkgsMusl.libffi.overrideAttrs (oldAttrs: {
            dontDisableStatic = true;
            configureFlags = (oldAttrs.configureFlags or [ ])
              ++ [ "--enable-static" "--disable-shared" ];
          });
        in pkgsMusl.haskell-nix.stackProject {
          src = pkgsMusl.haskell-nix.haskellLib.cleanGit {
            name = "tenpureto";
            src = ./.;
          };
          modules = modules ++ [
            { doHaddock = false; }
            ({ pkgs, ... }: {
              ghc.package =
                pkgs.buildPackages.haskell-nix.compiler.ghc883.override {
                  enableIntegerSimple = true;
                  enableShared = true;
                };
              packages.bytestring.flags.integer-simple = true;
              packages.text.flags.integer-simple = true;
              packages.scientific.flags.integer-simple = true;
              packages.integer-logarithms.flags.integer-gmp = false;
              packages.cryptonite.flags.integer-gmp = false;
              packages.hashable.flags.integer-gmp = false;
            })
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

      staticDrv = system:
        (staticProject system).tenpureto.components.exes.tenpureto;

      shell = system:
        (project system).shellFor {
          exactDeps = true;
          withHoogle = true;
          #tools = { brittany = "0.13.1.0"; };
        };

    in {
      packages.x86_64-darwin = { tenpureto = drv "x86_64-darwin"; };
      packages.x86_64-linux = {
        tenpureto = drv "x86_64-linux";
        tenpureto-static = staticDrv "x86_64-linux";
      };

      defaultPackage.x86_64-darwin = self.packages.x86_64-darwin.tenpureto;
      defaultPackage.x86_64-linux = self.packages.x86_64-linux.tenpureto;
    };

}
