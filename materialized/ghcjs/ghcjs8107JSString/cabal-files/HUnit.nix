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
      specVersion = "1.12";
      identifier = { name = "HUnit"; version = "1.6.2.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Simon Hengel <sol@typeful.net>";
      author = "Dean Herington";
      homepage = "https://github.com/hspec/HUnit#readme";
      url = "";
      synopsis = "A unit testing framework for Haskell";
      description = "HUnit is a unit testing framework for Haskell, inspired by the\nJUnit tool for Java, see: <http://www.junit.org>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."call-stack" or (errorHandler.buildDepError "call-stack"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."call-stack" or (errorHandler.buildDepError "call-stack"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/HUnit-1.6.2.0.tar.gz";
      sha256 = "b0b7538871ffc058486fc00740886d2f3172f8fa6869936bfe83a5e10bd744ab";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\n\n-- This file has been generated from package.yaml by hpack version 0.34.3.\n--\n-- see: https://github.com/sol/hpack\n\nname:                   HUnit\nversion:                1.6.2.0\nlicense:                BSD3\nlicense-file:           LICENSE\nauthor:                 Dean Herington\nmaintainer:             Simon Hengel <sol@typeful.net>\nstability:              stable\nhomepage:               https://github.com/hspec/HUnit#readme\nbug-reports:            https://github.com/hspec/HUnit/issues\ncategory:               Testing\nsynopsis:               A unit testing framework for Haskell\ndescription:            HUnit is a unit testing framework for Haskell, inspired by the\n                        JUnit tool for Java, see: <http://www.junit.org>.\nbuild-type:             Simple\nextra-source-files:\n  CHANGELOG.md\n  README.md\n\nsource-repository head\n  type: git\n  location: https://github.com/hspec/HUnit\n\nlibrary\n  hs-source-dirs:\n    src\n  build-depends:\n    base ==4.*,\n    call-stack >=0.3.0,\n    deepseq\n  exposed-modules:\n    Test.HUnit.Base\n    Test.HUnit.Lang\n    Test.HUnit.Terminal\n    Test.HUnit.Text\n    Test.HUnit\n  other-modules:\n    Paths_HUnit\n  default-language: Haskell2010\n  ghc-options: -Wall\n\ntest-suite tests\n  type: exitcode-stdio-1.0\n  main-is: HUnitTests.hs\n  hs-source-dirs:\n    tests\n    examples\n  build-depends:\n    HUnit,\n    base ==4.*,\n    call-stack >=0.3.0,\n    deepseq,\n    filepath\n  other-modules:\n    HUnitTestBase\n    HUnitTestExtended\n    TerminalTest\n    Example\n    Paths_HUnit\n  default-language: Haskell2010\n  ghc-options: -Wall\n";
    }