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
      identifier = { name = "edit-distance"; version = "0.2.2.1"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2008-2013 Maximilian Bolinbroke";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "Max Bolingbroke <batterseapower@hotmail.com>";
      homepage = "http://github.com/phadej/edit-distance";
      url = "";
      synopsis = "Levenshtein and restricted Damerau-Levenshtein edit distances";
      description = "Optimized edit distances for fuzzy matching, including Levenshtein and restricted Damerau-Levenshtein algorithms.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          ];
        buildable = true;
        };
      tests = {
        "edit-distance-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "edit-distance-benchmark" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/edit-distance-0.2.2.1.tar.gz";
      sha256 = "3e8885ee2f56ad4da940f043ae8f981ee2fe336b5e8e4ba3f7436cff4f526c4a";
      });
    }) // {
    package-description-override = "name:                edit-distance\r\nversion:             0.2.2.1\r\nx-revision: 1\r\ncabal-version:       >= 1.10\r\ncategory:            Algorithms\r\nsynopsis:            Levenshtein and restricted Damerau-Levenshtein edit distances\r\ndescription:         Optimized edit distances for fuzzy matching, including Levenshtein and restricted Damerau-Levenshtein algorithms.\r\nlicense:             BSD3\r\nlicense-File:        LICENSE\r\nextra-source-files:  README.md\r\nauthor:              Max Bolingbroke <batterseapower@hotmail.com>\r\ncopyright:           (c) 2008-2013 Maximilian Bolinbroke\r\nmaintainer:          Oleg Grenrus <oleg.grenrus@iki.fi>\r\nhomepage:            http://github.com/phadej/edit-distance\r\nbuild-type:          Simple\r\n\r\nlibrary\r\n  default-language:       Haskell98\r\n  exposed-modules:        Text.EditDistance\r\n  other-modules:          Text.EditDistance.EditCosts\r\n                          Text.EditDistance.SquareSTUArray\r\n                          Text.EditDistance.STUArray\r\n                          Text.EditDistance.Bits\r\n                          Text.EditDistance.MonadUtilities\r\n                          Text.EditDistance.ArrayUtilities\r\n  build-depends:          base >= 4.5 && < 5, array >= 0.1, random >= 1.0, containers >= 0.1.0.1\r\n  ghc-options:            -O2 -Wall\r\n\r\ntest-suite edit-distance-tests\r\n  default-language:       Haskell98\r\n  main-is:                Text/EditDistance/Tests.hs\r\n  other-modules:          Text.EditDistance.Tests.EditOperationOntology\r\n                          Text.EditDistance.Tests.Properties\r\n  type:                   exitcode-stdio-1.0\r\n  ghc-options:            -O2 -Wall\r\n  build-depends:          base >= 4.5 && < 5, array >= 0.1, random >= 1.0, containers >= 0.1.0.1,\r\n                          test-framework >= 0.1.1, QuickCheck >= 2.4 && <2.10, test-framework-quickcheck2\r\n\r\nbenchmark edit-distance-benchmark\r\n  default-language:       Haskell98\r\n  main-is:                Text/EditDistance/Benchmark.hs\r\n  type:                   exitcode-stdio-1.0\r\n  build-depends:          base >= 4.5 && < 5, array >= 0.1, random >= 1.0, time >= 1.0, process >= 1.0,\r\n                          deepseq >= 1.2, unix >= 2.3, criterion >= 1.1, containers >= 0.1.0.1\r\n  ghc-options:            -O2\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/phadej/edit-distance.git\r\n";
    }