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
      specVersion = "1.24";
      identifier = { name = "lifted-async"; version = "0.10.1.3"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2012-2021 Mitsutoshi Aoe";
      maintainer = "Mitsutoshi Aoe <me@maoe.name>";
      author = "Mitsutoshi Aoe";
      homepage = "https://github.com/maoe/lifted-async";
      url = "";
      synopsis = "Run lifted IO operations asynchronously and wait for their results";
      description = "This package provides IO operations from @async@ package lifted to any\ninstance of 'MonadBase' or 'MonadBaseControl'.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          ] ++ [
          (hsPkgs."constraints" or (errorHandler.buildDepError "constraints"))
          ];
        buildable = true;
        };
      tests = {
        "test-lifted-async" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."lifted-async" or (errorHandler.buildDepError "lifted-async"))
            (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-expected-failure" or (errorHandler.buildDepError "tasty-expected-failure"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-th" or (errorHandler.buildDepError "tasty-th"))
            ];
          buildable = true;
          };
        "regression-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."lifted-async" or (errorHandler.buildDepError "lifted-async"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-th" or (errorHandler.buildDepError "tasty-th"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "benchmark-lifted-async" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."lifted-async" or (errorHandler.buildDepError "lifted-async"))
            ];
          buildable = true;
          };
        "benchmark-lifted-async-threaded" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."lifted-async" or (errorHandler.buildDepError "lifted-async"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/lifted-async-0.10.1.3.tar.gz";
      sha256 = "f340fa9b649dd6bd3fc0942eceb94945a5b251e676b8d8e9841d6b24c531b4c2";
      });
    }) // {
    package-description-override = "cabal-version:       1.24\nname:                lifted-async\nversion:             0.10.1.3\nsynopsis:            Run lifted IO operations asynchronously and wait for their results\nhomepage:            https://github.com/maoe/lifted-async\nbug-reports:         https://github.com/maoe/lifted-async/issues\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Mitsutoshi Aoe\nmaintainer:          Mitsutoshi Aoe <me@maoe.name>\ncopyright:           Copyright (C) 2012-2021 Mitsutoshi Aoe\ncategory:            Concurrency\nbuild-type:          Simple\ntested-with:\n  GHC == 9.0.1\n  GHC == 8.10.3\n  GHC == 8.8.3\n  GHC == 8.6.5\n  GHC == 8.4.4\n  GHC == 8.2.2\n  GHC == 8.0.2\n  GHC == 7.10.3\n\nextra-source-files:\n  README.md\n  CHANGELOG.md\n\ndescription:\n  This package provides IO operations from @async@ package lifted to any\n  instance of 'MonadBase' or 'MonadBaseControl'.\n\nlibrary\n  exposed-modules:\n    Control.Concurrent.Async.Lifted\n    Control.Concurrent.Async.Lifted.Safe\n  build-depends:\n      base >= 4.5 && < 4.16\n    , async >= 2.2 && < 2.3\n    , lifted-base >= 0.2 && < 0.3\n    , transformers-base >= 0.4 && < 0.5\n    , monad-control == 1.0.*\n  if impl(ghc >= 7.8)\n    build-depends: constraints >= 0.2 && < 0.14\n  else\n    build-depends: constraints >= 0.2 && < 0.6\n  ghc-options: -Wall\n  hs-source-dirs: src\n  default-language: Haskell2010\n\ntest-suite test-lifted-async\n  type: exitcode-stdio-1.0\n  hs-source-dirs: tests\n  main-is: TestSuite.hs\n  other-modules:\n    Test.Async.Common\n    Test.Async.IO\n    Test.Async.State\n    Test.Async.Reader\n  ghc-options: -Wall -threaded\n  build-depends:\n      base\n    , HUnit\n    , lifted-async\n    , lifted-base\n    , monad-control\n    , mtl\n    , tasty\n    , tasty-expected-failure < 0.13\n    , tasty-hunit >= 0.9 && < 0.11\n    , tasty-th\n  default-language: Haskell2010\n\ntest-suite regression-tests\n  type: exitcode-stdio-1.0\n  hs-source-dirs: tests\n  main-is: RegressionTests.hs\n  ghc-options: -Wall -threaded\n  build-depends:\n      base\n    , async\n    , lifted-async\n    , mtl\n    , tasty-hunit >= 0.9 && < 0.11\n    , tasty-th\n  default-language: Haskell2010\n\nbenchmark benchmark-lifted-async\n  type: exitcode-stdio-1.0\n  hs-source-dirs: benchmarks\n  main-is: Benchmarks.hs\n  ghc-options: -Wall\n  build-depends:\n      base\n    , async\n    , tasty-bench < 0.3\n    , deepseq\n    , lifted-async\n  default-language: Haskell2010\n\nbenchmark benchmark-lifted-async-threaded\n  type: exitcode-stdio-1.0\n  hs-source-dirs: benchmarks\n  main-is: Benchmarks.hs\n  ghc-options: -Wall -threaded\n  build-depends:\n      base\n    , async\n    , tasty-bench < 0.3\n    , deepseq\n    , lifted-async\n  default-language: Haskell2010\n\nsource-repository head\n  type: git\n  branch: develop\n  location: https://github.com/maoe/lifted-async.git\n";
    }