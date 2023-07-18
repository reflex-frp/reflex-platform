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
      specVersion = "1.10";
      identifier = { name = "safe-exceptions"; version = "0.1.7.3"; };
      license = "MIT";
      copyright = "2016 FP Complete";
      maintainer = "michael@fpcomplete.com";
      author = "Michael Snoyman";
      homepage = "https://github.com/fpco/safe-exceptions#readme";
      url = "";
      synopsis = "Safe, consistent, and easy exception handling";
      description = "Please see README.md";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      tests = {
        "safe-exceptions-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."void" or (errorHandler.buildDepError "void"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/safe-exceptions-0.1.7.3.tar.gz";
      sha256 = "91ce28d8f8a6efd31788d4827ed5cdcb9a546ad4053a86c56f7947c66a30b5bf";
      });
    }) // {
    package-description-override = "name:                safe-exceptions\nversion:             0.1.7.3\nsynopsis:            Safe, consistent, and easy exception handling\ndescription:         Please see README.md\nhomepage:            https://github.com/fpco/safe-exceptions#readme\nlicense:             MIT\nlicense-file:        LICENSE\nauthor:              Michael Snoyman\nmaintainer:          michael@fpcomplete.com\ncopyright:           2016 FP Complete\ncategory:            Control\nbuild-type:          Simple\nextra-source-files:  README.md ChangeLog.md COOKBOOK.md\ncabal-version:       >=1.10\n\nlibrary\n  hs-source-dirs:      src\n  exposed-modules:     Control.Exception.Safe\n  build-depends:       base >= 4.11 && < 5\n                     , deepseq >= 1.2 && < 1.5\n                     , exceptions >= 0.10 && < 0.11\n                     , transformers >= 0.2 && < 0.7\n  default-language:    Haskell2010\n\ntest-suite safe-exceptions-test\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  main-is:             Spec.hs\n  other-modules:       Control.Exception.SafeSpec\n  build-depends:       base\n                     , hspec\n                     , safe-exceptions\n                     , transformers\n                     , void\n  ghc-options:         -threaded -rtsopts -with-rtsopts=-N\n  default-language:    Haskell2010\n\nsource-repository head\n  type:     git\n  location: https://github.com/fpco/safe-exceptions\n";
    }