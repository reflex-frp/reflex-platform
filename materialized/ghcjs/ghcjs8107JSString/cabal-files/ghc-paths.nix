{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = {};
    package = {
      specVersion = "1.6";
      identifier = { name = "ghc-paths"; version = "0.1.0.12"; };
      license = "BSD-3-Clause";
      copyright = "(c) Simon Marlow";
      maintainer = "Simon Marlow <marlowsd@gmail.com>";
      author = "Simon Marlow";
      homepage = "";
      url = "";
      synopsis = "Knowledge of GHC's installation directories";
      description = "Knowledge of GHC's installation directories";
      buildType = "Custom";
      setup-depends = [
        (hsPkgs.buildPackages.base or (pkgs.buildPackages.base or (errorHandler.setupDepError "base")))
        (hsPkgs.buildPackages.Cabal or (pkgs.buildPackages.Cabal or (errorHandler.setupDepError "Cabal")))
        (hsPkgs.buildPackages.directory or (pkgs.buildPackages.directory or (errorHandler.setupDepError "directory")))
        ];
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/ghc-paths-0.1.0.12.tar.gz";
      sha256 = "6ecbe676d073cb07989c61ce4c5709c4e67cbefdd2d55a4095f9388b6fe2c484";
      });
    }) // {
    package-description-override = "name: ghc-paths\r\nversion: 0.1.0.12\r\nx-revision: 2\r\nlicense: BSD3\r\nlicense-file: LICENSE\r\ncopyright: (c) Simon Marlow\r\nauthor: Simon Marlow\r\nmaintainer: Simon Marlow <marlowsd@gmail.com>\r\nstability: stable\r\nsynopsis: Knowledge of GHC's installation directories\r\ndescription: Knowledge of GHC's installation directories\r\ncategory: Development\r\ncabal-version: >= 1.6\r\nbuild-type: Custom\r\n\r\ncustom-setup\r\n        setup-depends: base >= 3 && < 5, Cabal >= 1.6 && <3.5, directory\r\n\r\nlibrary\r\n        build-depends: base >= 3 && < 5\r\n        exposed-modules: GHC.Paths\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/simonmar/ghc-paths\r\n";
    }