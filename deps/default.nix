{ system ? null }:
let extendHaskellPackages = nixpkgs: haskellPackages:
      with nixpkgs.haskell.lib; haskellPackages.override {
      overrides = self: super: {
        reflex = self.callPackage ./reflex {
          inherit (super) reflex;
          inherit overrideCabal;
        };
        reflex-dom = self.callPackage ./reflex-dom {
          inherit (super) reflex-dom;
          inherit (nixpkgs) fetchFromGitHub;
        };
        reflex-todomvc = self.callPackage ./reflex-todomvc {
          inherit (super) reflex-todomvc;
          inherit (nixpkgs) fetchFromGitHub;
        };
        active = overrideCabal super.active (drv: {
          version = "0.1.0.19";
          sha256 = "1zzzrjpfwxzf0zbz8vcnpfqi7djvrfxglhkvw1s6yj5gcblg2rcw";
          doCheck = false;
        });
        thyme = overrideCabal super.thyme (drv: {
          doCheck = false;
        });
        orgmode-parse = overrideCabal super.orgmode-parse (with self; drv: {
          version = "0.1.0.4";
          sha256 = "098zl8nyph459zyla0y2mkqiy78zp74yzadrnwa6xv07i5zs125h";
          buildDepends = [
            aeson attoparsec free hashable text thyme unordered-containers
          ];
          testDepends = [
            aeson attoparsec hashable HUnit tasty tasty-hunit text thyme
            unordered-containers
          ];
          doCheck = false;
        });
        HaskellForMaths = overrideCabal super.HaskellForMaths (drv: {
          version = "0.4.8";
          sha256 = "0yn2nj6irmj24j1djvnnq26i2lbf9g9x1wdhmcrk519glcn5k64j";
          buildDepends = [ self.semigroups ] ++ drv.buildDepends; # For some reason, without the spurious import of self.semigroups, HaskellForMaths will fail to build the environment for HaskellForMaths on ghcjs (it works on ghc)
        });
      };
    };
    extendBothHaskellPackages = super: let
      # Should be a fixpoint so we can replace original. see nixpkgs #7659
      self = super // {
        reflexPackages.ghc7101 = extendHaskellPackages self super.haskell-ng.packages.ghc7101;
        reflexPackages.ghcjs = extendHaskellPackages self super.haskell-ng.packages.ghcjs;
      };
      in self;

    stockNixpkgs = import ./nixpkgs ({
      config.allowUnfree = true;
    } // (if system == null then {} else { inherit system; }));

in extendBothHaskellPackages stockNixpkgs
