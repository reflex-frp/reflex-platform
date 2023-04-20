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
    flags = { bench = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "async"; version = "2.2.4"; };
      license = "BSD-3-Clause";
      copyright = "(c) Simon Marlow 2012";
      maintainer = "Simon Marlow <marlowsd@gmail.com>";
      author = "Simon Marlow";
      homepage = "https://github.com/simonmar/async";
      url = "";
      synopsis = "Run IO operations asynchronously and wait for their results";
      description = "This package provides a higher-level interface over\nthreads, in which an @Async a@ is a concurrent\nthread that will eventually deliver a value of\ntype @a@.  The package provides ways to create\n@Async@ computations, wait for their results, and\ncancel them.\n\nUsing @Async@ is safer than using threads in two\nways:\n\n* When waiting for a thread to return a result,\nif the thread dies with an exception then the\ncaller must either re-throw the exception\n('wait') or handle it ('waitCatch'); the\nexception cannot be ignored.\n\n* The API makes it possible to build a tree of\nthreads that are automatically killed when\ntheir parent dies (see 'withAsync').";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          ];
        buildable = true;
        };
      exes = {
        "concasync" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            ];
          buildable = if !flags.bench then false else true;
          };
        "conccancel" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            ];
          buildable = if !flags.bench then false else true;
          };
        "race" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            ];
          buildable = if !flags.bench then false else true;
          };
        };
      tests = {
        "test-async" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/async-2.2.4.tar.gz";
      sha256 = "484df85be0e76c4fed9376451e48e1d0c6e97952ce79735b72d54297e7e0a725";
      });
    }) // {
    package-description-override = "name:                async\nversion:             2.2.4\n-- don't forget to update ./changelog.md!\nx-revision:          2\nsynopsis:            Run IO operations asynchronously and wait for their results\n\ndescription:\n This package provides a higher-level interface over\n threads, in which an @Async a@ is a concurrent\n thread that will eventually deliver a value of\n type @a@.  The package provides ways to create\n @Async@ computations, wait for their results, and\n cancel them.\n .\n Using @Async@ is safer than using threads in two\n ways:\n .\n * When waiting for a thread to return a result,\n   if the thread dies with an exception then the\n   caller must either re-throw the exception\n   ('wait') or handle it ('waitCatch'); the\n   exception cannot be ignored.\n .\n * The API makes it possible to build a tree of\n   threads that are automatically killed when\n   their parent dies (see 'withAsync').\n\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Simon Marlow\nmaintainer:          Simon Marlow <marlowsd@gmail.com>\ncopyright:           (c) Simon Marlow 2012\ncategory:            Concurrency\nbuild-type:          Simple\ncabal-version:       >=1.10\nhomepage:            https://github.com/simonmar/async\nbug-reports:         https://github.com/simonmar/async/issues\ntested-with:\n    GHC == 9.4.1\n    GHC == 9.2.4\n    GHC == 9.0.2\n    GHC == 8.10.7\n    GHC == 8.8.4\n    GHC == 8.6.5\n    GHC == 8.4.4\n    GHC == 8.2.2\n    GHC == 8.0.2\n    GHC == 7.10.3\n    GHC == 7.8.4\n    GHC == 7.6.3\n    GHC == 7.4.2\n    GHC == 7.2.2\n    GHC == 7.0.4\n\nextra-source-files:\n    changelog.md\n    bench/race.hs\n\nsource-repository head\n    type: git\n    location: https://github.com/simonmar/async.git\n\nlibrary\n    default-language:    Haskell2010\n    other-extensions:    CPP, MagicHash, RankNTypes, UnboxedTuples\n    if impl(ghc>=7.1)\n        other-extensions: Trustworthy\n    exposed-modules:     Control.Concurrent.Async\n    build-depends:       base     >= 4.3     && < 4.18,\n                         hashable >= 1.1.2.0 && < 1.5,\n                         stm      >= 2.2     && < 2.6\n\ntest-suite test-async\n    default-language: Haskell2010\n    type:       exitcode-stdio-1.0\n    hs-source-dirs: test\n    main-is:    test-async.hs\n    build-depends: base,\n                   async,\n                   stm,\n                   test-framework,\n                   test-framework-hunit,\n                   HUnit\n\nflag bench\n    default: False\n\nexecutable concasync\n    if !flag(bench)\n       buildable: False\n    default-language: Haskell2010\n    hs-source-dirs: bench\n    main-is:    concasync.hs\n    build-depends: base, async, stm\n    ghc-options: -O2\n\nexecutable conccancel\n    if !flag(bench)\n       buildable: False\n    default-language: Haskell2010\n    hs-source-dirs: bench\n    main-is:    conccancel.hs\n    build-depends: base, async, stm\n    ghc-options: -O2 -threaded\n\nexecutable race\n    if !flag(bench)\n       buildable: False\n    default-language: Haskell2010\n    hs-source-dirs: bench\n    main-is:    race.hs\n    build-depends: base, async, stm\n    ghc-options: -O2 -threaded\n";
    }