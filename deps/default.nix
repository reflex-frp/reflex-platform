{ system ? null }:

let
  localPkgs = import <nixpkgs> {};

  extendHaskellPackages = haskellLib: haskellPackages:
    with haskellLib; haskellPackages.override {
    overrides = self: super: {
      reflex = self.callPackage ./reflex {};
      reflex-dom = self.callPackage ./reflex-dom {};
      reflex-todomvc = self.callPackage ./reflex-todomvc {};
      ghcjs-jquery = self.callPackage ({ mkDerivation, data-default, ghcjs-base, ghcjs-dom, text }:
        mkDerivation {
          pname = "ghcjs-jquery";
          version = "0.1.0.0";
          src = localPkgs.fetchgit {
            url = git://github.com/ghcjs/ghcjs-jquery;
            rev = "c5eeeafcf81c0d3237b8b9fcb98c4b3633a1297f";
            sha256 = "3b2de54224963ee17857a9737b65d49edc423e06ad7e9c9b85d9f69ca923676a";
          };
          buildDepends = [
            data-default ghcjs-base ghcjs-dom text
          ];
          jailbreak = true;
          license = null;
        }
      ) {};
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
      twitter-types = overrideCabal super.twitter-types (drv: {
        doCheck = false;
      });
      twitter-types-lens = overrideCabal super.twitter-types-lens (drv: {
        doCheck = false;
      });
      HaskellForMaths = overrideCabal super.HaskellForMaths (drv: {
        version = "0.4.8";
        sha256 = "0yn2nj6irmj24j1djvnnq26i2lbf9g9x1wdhmcrk519glcn5k64j";
        buildDepends = [ self.semigroups ] ++ drv.buildDepends; # For some reason, without the spurious import of self.semigroups, HaskellForMaths will fail to build the environment for HaskellForMaths on ghcjs (it works on ghc)
      });
      dependent-sum-template = overrideCabal super.dependent-sum-template (drv: {
        version = "0.0.0.4";
        src = localPkgs.fetchgit {
          url = git://github.com/ryantrinkle/dependent-sum-template;
          rev = "abcd0f01a3e264e5bc1f3b00f3d03082f091ec49";
          sha256 = "16f95348c559394a39848394a9e1aa8318c79bfc62bc6946edad9aabd20a8e2d";
        };
      });
    };
  };

  makeReflexPkgSet = nixpkgs: let
      f = extendHaskellPackages nixpkgs.haskell-ng.lib;
    in {
      inherit nixpkgs;
      ghc7101 = f nixpkgs.haskell-ng.packages.ghc7101;
      ghcjs = f nixpkgs.haskell-ng.packages.ghcjs;
    };

  stockNixpkgs = import ./nixpkgs ({
    config.allowUnfree = true;
  } // (if system == null then {} else { inherit system; }));

in makeReflexPkgSet stockNixpkgs
