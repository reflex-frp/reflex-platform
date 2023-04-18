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
      identifier = { name = "random"; version = "1.2.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "core-libraries-committee@haskell.org";
      author = "";
      homepage = "";
      url = "";
      synopsis = "Pseudo-random number generation";
      description = "This package provides basic pseudo-random number generation, including the\nability to split random number generators.\n\n== \"System.Random\": pure pseudo-random number interface\n\nIn pure code, use 'System.Random.uniform' and 'System.Random.uniformR' from\n\"System.Random\" to generate pseudo-random numbers with a pure pseudo-random\nnumber generator like 'System.Random.StdGen'.\n\nAs an example, here is how you can simulate rolls of a six-sided die using\n'System.Random.uniformR':\n\n>>> let roll = uniformR (1, 6)        :: RandomGen g => g -> (Word, g)\n>>> let rolls = unfoldr (Just . roll) :: RandomGen g => g -> [Word]\n>>> let pureGen = mkStdGen 42\n>>> take 10 (rolls pureGen)           :: [Word]\n[1,1,3,2,4,5,3,4,6,2]\n\nSee \"System.Random\" for more details.\n\n== \"System.Random.Stateful\": monadic pseudo-random number interface\n\nIn monadic code, use 'System.Random.Stateful.uniformM' and\n'System.Random.Stateful.uniformRM' from \"System.Random.Stateful\" to generate\npseudo-random numbers with a monadic pseudo-random number generator, or\nusing a monadic adapter.\n\nAs an example, here is how you can simulate rolls of a six-sided die using\n'System.Random.Stateful.uniformRM':\n\n>>> let rollM = uniformRM (1, 6)                 :: StatefulGen g m => g -> m Word\n>>> let pureGen = mkStdGen 42\n>>> runStateGen_ pureGen (replicateM 10 . rollM) :: [Word]\n[1,1,3,2,4,5,3,4,6,2]\n\nThe monadic adapter 'System.Random.Stateful.runGenState_' is used here to lift\nthe pure pseudo-random number generator @pureGen@ into the\n'System.Random.Stateful.StatefulGen' context.\n\nThe monadic interface can also be used with existing monadic pseudo-random\nnumber generators. In this example, we use the one provided in the\n<https://hackage.haskell.org/package/mwc-random mwc-random> package:\n\n>>> import System.Random.MWC as MWC\n>>> let rollM = uniformRM (1, 6)       :: StatefulGen g m => g -> m Word\n>>> monadicGen <- MWC.create\n>>> replicateM 10 (rollM monadicGen) :: IO [Word]\n[2,3,6,6,4,4,3,1,5,4]\n\nSee \"System.Random.Stateful\" for more details.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"));
        buildable = true;
        };
      tests = {
        "legacy-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            ];
          buildable = true;
          };
        "doctests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."mwc-random" or (errorHandler.buildDepError "mwc-random"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."smallcheck" or (errorHandler.buildDepError "smallcheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-smallcheck" or (errorHandler.buildDepError "tasty-smallcheck"))
            (hsPkgs."tasty-expected-failure" or (errorHandler.buildDepError "tasty-expected-failure"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "legacy-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."rdtsc" or (errorHandler.buildDepError "rdtsc"))
            (hsPkgs."split" or (errorHandler.buildDepError "split"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          };
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/random-1.2.0.tar.gz";
      sha256 = "e4519cf7c058bfd5bdbe4acc782284acc9e25e74487208619ca83cbcd63fb9de";
      });
    }) // {
    package-description-override = "cabal-version:      >=1.10\r\nname:               random\r\nversion:            1.2.0\r\nx-revision: 5\r\nlicense:            BSD3\r\nlicense-file:       LICENSE\r\nmaintainer:         core-libraries-committee@haskell.org\r\nbug-reports:        https://github.com/haskell/random/issues\r\nsynopsis:           Pseudo-random number generation\r\ndescription:\r\n    This package provides basic pseudo-random number generation, including the\r\n    ability to split random number generators.\r\n    .\r\n    == \"System.Random\": pure pseudo-random number interface\r\n    .\r\n    In pure code, use 'System.Random.uniform' and 'System.Random.uniformR' from\r\n    \"System.Random\" to generate pseudo-random numbers with a pure pseudo-random\r\n    number generator like 'System.Random.StdGen'.\r\n    .\r\n    As an example, here is how you can simulate rolls of a six-sided die using\r\n    'System.Random.uniformR':\r\n    .\r\n    >>> let roll = uniformR (1, 6)        :: RandomGen g => g -> (Word, g)\r\n    >>> let rolls = unfoldr (Just . roll) :: RandomGen g => g -> [Word]\r\n    >>> let pureGen = mkStdGen 42\r\n    >>> take 10 (rolls pureGen)           :: [Word]\r\n    [1,1,3,2,4,5,3,4,6,2]\r\n    .\r\n    See \"System.Random\" for more details.\r\n    .\r\n    == \"System.Random.Stateful\": monadic pseudo-random number interface\r\n    .\r\n    In monadic code, use 'System.Random.Stateful.uniformM' and\r\n    'System.Random.Stateful.uniformRM' from \"System.Random.Stateful\" to generate\r\n    pseudo-random numbers with a monadic pseudo-random number generator, or\r\n    using a monadic adapter.\r\n    .\r\n    As an example, here is how you can simulate rolls of a six-sided die using\r\n    'System.Random.Stateful.uniformRM':\r\n    .\r\n    >>> let rollM = uniformRM (1, 6)                 :: StatefulGen g m => g -> m Word\r\n    >>> let pureGen = mkStdGen 42\r\n    >>> runStateGen_ pureGen (replicateM 10 . rollM) :: [Word]\r\n    [1,1,3,2,4,5,3,4,6,2]\r\n    .\r\n    The monadic adapter 'System.Random.Stateful.runGenState_' is used here to lift\r\n    the pure pseudo-random number generator @pureGen@ into the\r\n    'System.Random.Stateful.StatefulGen' context.\r\n    .\r\n    The monadic interface can also be used with existing monadic pseudo-random\r\n    number generators. In this example, we use the one provided in the\r\n    <https://hackage.haskell.org/package/mwc-random mwc-random> package:\r\n    .\r\n    >>> import System.Random.MWC as MWC\r\n    >>> let rollM = uniformRM (1, 6)       :: StatefulGen g m => g -> m Word\r\n    >>> monadicGen <- MWC.create\r\n    >>> replicateM 10 (rollM monadicGen) :: IO [Word]\r\n    [2,3,6,6,4,4,3,1,5,4]\r\n    .\r\n    See \"System.Random.Stateful\" for more details.\r\n\r\ncategory:           System\r\nbuild-type:         Simple\r\nextra-source-files:\r\n    README.md\r\n    CHANGELOG.md\r\ntested-with:         GHC == 7.10.2\r\n                   , GHC == 7.10.3\r\n                   , GHC == 8.0.2\r\n                   , GHC == 8.2.2\r\n                   , GHC == 8.4.3\r\n                   , GHC == 8.4.4\r\n                   , GHC == 8.6.3\r\n                   , GHC == 8.6.4\r\n                   , GHC == 8.6.5\r\n                   , GHC == 8.8.1\r\n                   , GHC == 8.8.2\r\n                   , GHC == 8.10.1\r\n\r\nsource-repository head\r\n    type:     git\r\n    location: https://github.com/haskell/random.git\r\n\r\n\r\nlibrary\r\n    exposed-modules:\r\n        System.Random\r\n        System.Random.Internal\r\n        System.Random.Stateful\r\n\r\n    hs-source-dirs:   src\r\n    default-language: Haskell2010\r\n    ghc-options:\r\n        -Wall\r\n    if impl(ghc >= 8.0)\r\n        ghc-options:\r\n            -Wincomplete-record-updates -Wincomplete-uni-patterns\r\n\r\n    build-depends:\r\n        base >=4.8 && <5,\r\n        bytestring >=0.10.4 && <0.12,\r\n        deepseq >=1.1 && <2,\r\n        mtl >=2.2 && <2.3,\r\n        splitmix >=0.1 && <0.2\r\n    if impl(ghc < 8.0)\r\n       build-depends:\r\n           transformers\r\n\r\ntest-suite legacy-test\r\n    type:             exitcode-stdio-1.0\r\n    main-is:          Legacy.hs\r\n    hs-source-dirs:   test-legacy\r\n    other-modules:\r\n        T7936\r\n        TestRandomIOs\r\n        TestRandomRs\r\n        Random1283\r\n        RangeTest\r\n\r\n    default-language: Haskell2010\r\n    ghc-options:      -with-rtsopts=-M4M\r\n    if impl(ghc >= 8.0)\r\n        ghc-options:\r\n            -Wno-deprecations\r\n    build-depends:\r\n        base -any,\r\n        containers >=0.5 && <0.7,\r\n        random -any\r\n\r\ntest-suite doctests\r\n    type:             exitcode-stdio-1.0\r\n    main-is:          doctests.hs\r\n    hs-source-dirs:   test\r\n    default-language: Haskell2010\r\n    build-depends:\r\n        base -any,\r\n        doctest >=0.15 && <0.19,\r\n        mwc-random >=0.13 && <0.16,\r\n        primitive >=0.6 && <0.8,\r\n        random -any,\r\n        unliftio >=0.2 && <0.3,\r\n        vector >= 0.10 && <0.14\r\n\r\ntest-suite spec\r\n    type:             exitcode-stdio-1.0\r\n    main-is:          Spec.hs\r\n    hs-source-dirs:   test\r\n    other-modules:\r\n        Spec.Range\r\n        Spec.Run\r\n\r\n    default-language: Haskell2010\r\n    ghc-options:      -Wall\r\n    build-depends:\r\n        base -any,\r\n        bytestring -any,\r\n        random -any,\r\n        smallcheck >=1.2 && <1.3,\r\n        tasty >=1.0 && <1.5,\r\n        tasty-smallcheck >=0.8 && <0.9,\r\n        tasty-expected-failure -any,\r\n        tasty-hunit >=0.10 && <0.11\r\n\r\nbenchmark legacy-bench\r\n    type:             exitcode-stdio-1.0\r\n    main-is:          SimpleRNGBench.hs\r\n    hs-source-dirs:   bench-legacy\r\n    other-modules:    BinSearch\r\n    default-language: Haskell2010\r\n    ghc-options:\r\n        -Wall -O2 -threaded -rtsopts -with-rtsopts=-N\r\n    if impl(ghc >= 8.0)\r\n        ghc-options:\r\n            -Wno-deprecations\r\n\r\n    build-depends:\r\n        base -any,\r\n        random -any,\r\n        rdtsc -any,\r\n        split >=0.2 && <0.3,\r\n        time >=1.4 && <1.11\r\n\r\nbenchmark bench\r\n    type:             exitcode-stdio-1.0\r\n    main-is:          Main.hs\r\n    hs-source-dirs:   bench\r\n    default-language: Haskell2010\r\n    ghc-options:      -Wall -O2\r\n    build-depends:\r\n        base -any,\r\n        gauge >=0.2.3 && <0.3,\r\n        mtl,\r\n        random -any,\r\n        splitmix >=0.1 && <0.2\r\n";
    }