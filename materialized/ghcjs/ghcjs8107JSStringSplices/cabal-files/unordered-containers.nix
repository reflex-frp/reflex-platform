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
    flags = { debug = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "unordered-containers"; version = "0.2.13.0"; };
      license = "BSD-3-Clause";
      copyright = "2010-2014 Johan Tibell\n2010 Edward Z. Yang";
      maintainer = "johan.tibell@gmail.com, David.Feuer@gmail.com";
      author = "Johan Tibell";
      homepage = "https://github.com/haskell-unordered-containers/unordered-containers";
      url = "";
      synopsis = "Efficient hashing-based container types";
      description = "Efficient hashing-based container types.  The containers have been\noptimized for performance critical use, both in terms of large data\nquantities and high speed.\n\nThe declared cost of each operation is either worst-case or\namortized, but remains valid even if structures are shared.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          ];
        buildable = true;
        };
      tests = {
        "hashmap-lazy-properties" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            ];
          buildable = true;
          };
        "hashmap-strict-properties" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            ];
          buildable = true;
          };
        "hashset-properties" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            ];
          buildable = true;
          };
        "list-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            ];
          buildable = true;
          };
        "regressions" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            ];
          buildable = true;
          };
        "strictness-properties" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ChasingBottoms" or (errorHandler.buildDepError "ChasingBottoms"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "benchmarks" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."hashmap" or (errorHandler.buildDepError "hashmap"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/unordered-containers-0.2.13.0.tar.gz";
      sha256 = "86b01369ab8eb311383a052d389337e2cd71a63088323f02932754df4aa37b55";
      });
    }) // {
    package-description-override = "name:           unordered-containers\nversion:        0.2.13.0\nsynopsis:       Efficient hashing-based container types\ndescription:\n  Efficient hashing-based container types.  The containers have been\n  optimized for performance critical use, both in terms of large data\n  quantities and high speed.\n  .\n  The declared cost of each operation is either worst-case or\n  amortized, but remains valid even if structures are shared.\nlicense:        BSD3\nlicense-file:   LICENSE\nauthor:         Johan Tibell\nmaintainer:     johan.tibell@gmail.com, David.Feuer@gmail.com\nHomepage:       https://github.com/haskell-unordered-containers/unordered-containers\nbug-reports:    https://github.com/haskell-unordered-containers/unordered-containers/issues\ncopyright:      2010-2014 Johan Tibell\n                2010 Edward Z. Yang\ncategory:       Data\nbuild-type:     Simple\ncabal-version:  >=1.10\nextra-source-files: CHANGES.md\n\ntested-with:\n  GHC ==8.10.1\n   || ==8.8.3\n   || ==8.6.5\n   || ==8.4.4\n   || ==8.2.2\n   || ==8.0.2\n   || ==7.10.3\n   || ==7.8.4\n\nflag debug\n  description:  Enable debug support\n  default:      False\n\nlibrary\n  exposed-modules:\n    Data.HashMap.Internal\n    Data.HashMap.Internal.Array\n    Data.HashMap.Internal.List\n    Data.HashMap.Internal.Strict\n    Data.HashMap.Internal.Unsafe\n    Data.HashMap.Lazy\n    Data.HashMap.Strict\n    Data.HashSet\n    Data.HashSet.Internal\n\n  build-depends:\n    base >= 4.7 && < 5,\n    deepseq >= 1.1,\n    hashable >= 1.0.1.1 && < 1.4\n\n  default-language: Haskell2010\n\n  other-extensions:\n    RoleAnnotations,\n    UnboxedTuples,\n    ScopedTypeVariables,\n    MagicHash,\n    BangPatterns\n\n  ghc-options: -Wall -O2 -fwarn-tabs -ferror-spans\n\n  if impl (ghc < 8.2)\n    -- This is absolutely necessary (but not sufficient) for correctness due to\n    -- the referential-transparency-breaking mutability in unsafeInsertWith. See\n    -- #147 and GHC #13615 for details. The bug was fixed in GHC 8.2.\n    ghc-options: -feager-blackholing\n  if flag(debug)\n    cpp-options: -DASSERTS\n\ntest-suite hashmap-lazy-properties\n  hs-source-dirs: tests\n  main-is: HashMapProperties.hs\n  type: exitcode-stdio-1.0\n\n  build-depends:\n    base,\n    containers >= 0.5.8,\n    hashable >= 1.0.1.1,\n    QuickCheck >= 2.4.0.1,\n    test-framework >= 0.3.3,\n    test-framework-quickcheck2 >= 0.2.9,\n    unordered-containers\n\n  default-language: Haskell2010\n  ghc-options: -Wall\n  cpp-options: -DASSERTS\n\ntest-suite hashmap-strict-properties\n  hs-source-dirs: tests\n  main-is: HashMapProperties.hs\n  type: exitcode-stdio-1.0\n\n  build-depends:\n    base,\n    containers >= 0.5.8,\n    hashable >= 1.0.1.1,\n    QuickCheck >= 2.4.0.1,\n    test-framework >= 0.3.3,\n    test-framework-quickcheck2 >= 0.2.9,\n    unordered-containers\n\n  default-language: Haskell2010\n  ghc-options: -Wall\n  cpp-options: -DASSERTS -DSTRICT\n\ntest-suite hashset-properties\n  hs-source-dirs: tests\n  main-is: HashSetProperties.hs\n  type: exitcode-stdio-1.0\n\n  build-depends:\n    base,\n    containers >= 0.4,\n    hashable >= 1.0.1.1,\n    QuickCheck >= 2.4.0.1,\n    test-framework >= 0.3.3,\n    test-framework-quickcheck2 >= 0.2.9,\n    unordered-containers\n\n  default-language: Haskell2010\n  ghc-options: -Wall\n  cpp-options: -DASSERTS\n\ntest-suite list-tests\n  hs-source-dirs: tests .\n  main-is: List.hs\n  other-modules:\n    Data.HashMap.Internal.List\n  type: exitcode-stdio-1.0\n\n  build-depends:\n    base,\n    containers >= 0.4,\n    QuickCheck >= 2.4.0.1,\n    test-framework >= 0.3.3,\n    test-framework-quickcheck2 >= 0.2.9\n\n  default-language: Haskell2010\n  ghc-options: -Wall\n  cpp-options: -DASSERTS\n\ntest-suite regressions\n  hs-source-dirs: tests\n  main-is: Regressions.hs\n  type: exitcode-stdio-1.0\n\n  build-depends:\n    base,\n    hashable >= 1.0.1.1,\n    HUnit,\n    QuickCheck >= 2.4.0.1,\n    random,\n    test-framework >= 0.3.3,\n    test-framework-hunit,\n    test-framework-quickcheck2,\n    unordered-containers\n\n  default-language: Haskell2010\n  ghc-options: -Wall\n  cpp-options: -DASSERTS\n\ntest-suite strictness-properties\n  hs-source-dirs: tests\n  main-is: Strictness.hs\n  type: exitcode-stdio-1.0\n\n  build-depends:\n    base,\n    ChasingBottoms,\n    containers >= 0.4.2,\n    hashable >= 1.0.1.1,\n    QuickCheck >= 2.4.0.1,\n    test-framework >= 0.3.3,\n    test-framework-quickcheck2 >= 0.2.9,\n    unordered-containers\n\n  default-language: Haskell2010\n  ghc-options: -Wall\n  cpp-options: -DASSERTS\n\nbenchmark benchmarks\n  hs-source-dirs: benchmarks\n  main-is: Benchmarks.hs\n  type: exitcode-stdio-1.0\n\n  other-modules:\n    Util.ByteString\n    Util.String\n    Util.Int\n\n  build-depends:\n    base >= 4.8.0,\n    bytestring,\n    containers,\n    gauge >= 0.2.5 && < 0.3,\n    deepseq >= 1.4,\n    hashable >= 1.0.1.1,\n    hashmap,\n    mtl,\n    random,\n    unordered-containers\n\n  default-language: Haskell2010\n  ghc-options: -Wall -O2 -rtsopts -fwarn-tabs -ferror-spans\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskell-unordered-containers/unordered-containers.git\n";
    }