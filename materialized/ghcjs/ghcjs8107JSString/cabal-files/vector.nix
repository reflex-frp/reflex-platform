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
    flags = {
      boundschecks = true;
      unsafechecks = false;
      internalchecks = false;
      wall = false;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "vector"; version = "0.12.2.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) Roman Leshchinskiy 2008-2012";
      maintainer = "Haskell Libraries Team <libraries@haskell.org>";
      author = "Roman Leshchinskiy <rl@cse.unsw.edu.au>";
      homepage = "https://github.com/haskell/vector";
      url = "";
      synopsis = "Efficient Arrays";
      description = "\nAn efficient implementation of Int-indexed arrays (both mutable\nand immutable), with a powerful loop optimisation framework .\n\nIt is structured as follows:\n\n[\"Data.Vector\"] Boxed vectors of arbitrary types.\n\n[\"Data.Vector.Unboxed\"] Unboxed vectors with an adaptive\nrepresentation based on data type families.\n\n[\"Data.Vector.Storable\"] Unboxed vectors of 'Storable' types.\n\n[\"Data.Vector.Primitive\"] Unboxed vectors of primitive types as\ndefined by the @primitive@ package. \"Data.Vector.Unboxed\" is more\nflexible at no performance cost.\n\n[\"Data.Vector.Generic\"] Generic interface to the vector types.\n\nThere is also a (draft) tutorial on common uses of vector.\n\n* <http://haskell.org/haskellwiki/Numeric_Haskell:_A_Vector_Tutorial>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).gt "8.0")) [
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          ];
        buildable = true;
        };
      tests = {
        "vector-tests-O0" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).gt "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = true;
          };
        "vector-tests-O2" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).gt "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = true;
          };
        "vector-doctest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = (if compiler.isGhc && (compiler.version).lt "8.6"
            then false
            else true) && (if compiler.isGhc && (compiler.version).ge "8.10" && (compiler.isGhc && (compiler.version).lt "8.11")
            then false
            else true);
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/vector-0.12.2.0.tar.gz";
      sha256 = "17ab0b84c87859333ff681bb9f768368779677925bd589ff4baa05be3fd26b50";
      });
    }) // {
    package-description-override = "Name:           vector\nVersion:        0.12.2.0\n-- don't forget to update the changelog file!\nLicense:        BSD3\nLicense-File:   LICENSE\nAuthor:         Roman Leshchinskiy <rl@cse.unsw.edu.au>\nMaintainer:     Haskell Libraries Team <libraries@haskell.org>\nCopyright:      (c) Roman Leshchinskiy 2008-2012\nHomepage:       https://github.com/haskell/vector\nBug-Reports:    https://github.com/haskell/vector/issues\nCategory:       Data, Data Structures\nSynopsis:       Efficient Arrays\nDescription:\n        .\n        An efficient implementation of Int-indexed arrays (both mutable\n        and immutable), with a powerful loop optimisation framework .\n        .\n        It is structured as follows:\n        .\n        [\"Data.Vector\"] Boxed vectors of arbitrary types.\n        .\n        [\"Data.Vector.Unboxed\"] Unboxed vectors with an adaptive\n        representation based on data type families.\n        .\n        [\"Data.Vector.Storable\"] Unboxed vectors of 'Storable' types.\n        .\n        [\"Data.Vector.Primitive\"] Unboxed vectors of primitive types as\n        defined by the @primitive@ package. \"Data.Vector.Unboxed\" is more\n        flexible at no performance cost.\n        .\n        [\"Data.Vector.Generic\"] Generic interface to the vector types.\n        .\n        There is also a (draft) tutorial on common uses of vector.\n        .\n        * <http://haskell.org/haskellwiki/Numeric_Haskell:_A_Vector_Tutorial>\n\nTested-With:\n  GHC == 7.4.2,\n  GHC == 7.6.3,\n  GHC == 7.8.4,\n  GHC == 7.10.3,\n  GHC == 8.0.2,\n  GHC == 8.2.2,\n  GHC == 8.4.4,\n  GHC == 8.6.5,\n  GHC == 8.8.1,\n  GHC == 8.10.1\n\n\nCabal-Version:  >=1.10\nBuild-Type:     Simple\n\nExtra-Source-Files:\n      changelog.md\n      README.md\n      tests/LICENSE\n      tests/Setup.hs\n      tests/Main.hs\n      benchmarks/vector-benchmarks.cabal\n      benchmarks/LICENSE\n      benchmarks/Setup.hs\n      benchmarks/Main.hs\n      benchmarks/Algo/AwShCC.hs\n      benchmarks/Algo/HybCC.hs\n      benchmarks/Algo/Leaffix.hs\n      benchmarks/Algo/ListRank.hs\n      benchmarks/Algo/Quickhull.hs\n      benchmarks/Algo/Rootfix.hs\n      benchmarks/Algo/Spectral.hs\n      benchmarks/Algo/Tridiag.hs\n      benchmarks/TestData/Graph.hs\n      benchmarks/TestData/ParenTree.hs\n      benchmarks/TestData/Random.hs\n      internal/GenUnboxTuple.hs\n      internal/unbox-tuple-instances\n\n\n\nFlag BoundsChecks\n  Description: Enable bounds checking\n  Default: True\n  Manual: True\n\nFlag UnsafeChecks\n  Description: Enable bounds checking in unsafe operations at the cost of a\n               significant performance penalty\n  Default: False\n  Manual: True\n\nFlag InternalChecks\n  Description: Enable internal consistency checks at the cost of a\n               significant performance penalty\n  Default: False\n  Manual: True\n\nFlag Wall\n  Description: Enable all -Wall warnings\n  Default: False\n  Manual: True\n\n\nLibrary\n  Default-Language: Haskell2010\n  Other-Extensions:\n        BangPatterns\n        CPP\n        DeriveDataTypeable\n        ExistentialQuantification\n        FlexibleContexts\n        FlexibleInstances\n        GADTs\n        KindSignatures\n        MagicHash\n        MultiParamTypeClasses\n        Rank2Types\n        ScopedTypeVariables\n        StandaloneDeriving\n        TypeFamilies\n\n  Exposed-Modules:\n        Data.Vector.Internal.Check\n\n        Data.Vector.Fusion.Util\n        Data.Vector.Fusion.Stream.Monadic\n        Data.Vector.Fusion.Bundle.Size\n        Data.Vector.Fusion.Bundle.Monadic\n        Data.Vector.Fusion.Bundle\n\n        Data.Vector.Generic.Mutable.Base\n        Data.Vector.Generic.Mutable\n        Data.Vector.Generic.Base\n        Data.Vector.Generic.New\n        Data.Vector.Generic\n\n        Data.Vector.Primitive.Mutable\n        Data.Vector.Primitive\n\n        Data.Vector.Storable.Internal\n        Data.Vector.Storable.Mutable\n        Data.Vector.Storable\n\n        Data.Vector.Unboxed.Base\n        Data.Vector.Unboxed.Mutable\n        Data.Vector.Unboxed\n\n        Data.Vector.Mutable\n        Data.Vector\n\n  Include-Dirs:\n        include, internal\n\n  Install-Includes:\n        vector.h\n\n  Build-Depends: base >= 4.5 && < 4.16\n               , primitive >= 0.6.4.0 && < 0.8\n               , ghc-prim >= 0.2 && < 0.8\n               , deepseq >= 1.1 && < 1.5\n  if !impl(ghc > 8.0)\n    Build-Depends: fail == 4.9.*\n                 , semigroups >= 0.18 && < 0.20\n\n  Ghc-Options: -O2 -Wall\n\n  if !flag(Wall)\n    Ghc-Options: -fno-warn-orphans\n\n    if impl(ghc >= 8.0) && impl(ghc < 8.1)\n      Ghc-Options:   -Wno-redundant-constraints\n\n  if flag(BoundsChecks)\n    cpp-options: -DVECTOR_BOUNDS_CHECKS\n\n  if flag(UnsafeChecks)\n    cpp-options: -DVECTOR_UNSAFE_CHECKS\n\n  if flag(InternalChecks)\n    cpp-options: -DVECTOR_INTERNAL_CHECKS\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskell/vector.git\n\n\n\ntest-suite vector-tests-O0\n  Default-Language: Haskell2010\n  type: exitcode-stdio-1.0\n  Main-Is:  Main.hs\n\n  other-modules: Boilerplater\n                 Tests.Bundle\n                 Tests.Move\n                 Tests.Vector\n                 Tests.Vector.Property\n                 Tests.Vector.Boxed\n                 Tests.Vector.Storable\n                 Tests.Vector.Primitive\n                 Tests.Vector.Unboxed\n                 Tests.Vector.UnitTests\n                 Utilities\n\n  hs-source-dirs: tests\n  Build-Depends: base >= 4.5 && < 5, template-haskell, base-orphans >= 0.6, vector,\n                 primitive, random,\n                 QuickCheck >= 2.9 && < 2.15, HUnit, tasty,\n                 tasty-hunit, tasty-quickcheck,\n                 transformers >= 0.2.0.0\n  if !impl(ghc > 8.0)\n    Build-Depends: semigroups\n\n  default-extensions: CPP,\n              ScopedTypeVariables,\n              PatternGuards,\n              MultiParamTypeClasses,\n              FlexibleContexts,\n              Rank2Types,\n              TypeSynonymInstances,\n              TypeFamilies,\n              TemplateHaskell\n\n  Ghc-Options: -O0 -threaded\n  Ghc-Options: -Wall\n\n  if !flag(Wall)\n    Ghc-Options: -fno-warn-orphans -fno-warn-missing-signatures\n    if impl(ghc >= 8.0) && impl( ghc < 8.1)\n      Ghc-Options: -Wno-redundant-constraints\n\n\ntest-suite vector-tests-O2\n  Default-Language: Haskell2010\n  type: exitcode-stdio-1.0\n  Main-Is:  Main.hs\n\n  other-modules: Boilerplater\n                 Tests.Bundle\n                 Tests.Move\n                 Tests.Vector\n                 Tests.Vector.Property\n                 Tests.Vector.Boxed\n                 Tests.Vector.Storable\n                 Tests.Vector.Primitive\n                 Tests.Vector.Unboxed\n                 Tests.Vector.UnitTests\n                 Utilities\n\n  hs-source-dirs: tests\n  Build-Depends: base >= 4.5 && < 5, template-haskell, base-orphans >= 0.6, vector,\n                 primitive, random,\n                 QuickCheck >= 2.9 && < 2.15, HUnit, tasty,\n                 tasty-hunit, tasty-quickcheck,\n                 transformers >= 0.2.0.0\n  if !impl(ghc > 8.0)\n    Build-Depends: semigroups\n\n  default-extensions: CPP,\n              ScopedTypeVariables,\n              PatternGuards,\n              MultiParamTypeClasses,\n              FlexibleContexts,\n              Rank2Types,\n              TypeSynonymInstances,\n              TypeFamilies,\n              TemplateHaskell\n\n\n  Ghc-Options: -Wall\n  Ghc-Options:  -O2 -threaded\n  if !flag(Wall)\n    Ghc-Options: -fno-warn-orphans -fno-warn-missing-signatures\n    if impl(ghc >= 8.0) && impl(ghc < 8.1)\n      Ghc-Options: -Wno-redundant-constraints\n\ntest-suite vector-doctest\n  type:             exitcode-stdio-1.0\n  main-is:          doctests.hs\n  hs-source-dirs:   tests\n  default-language: Haskell2010\n  -- Older GHC choke on {-# UNPACK #-} pragma for some reason\n  if impl(ghc < 8.6)\n    buildable: False\n  -- GHC 8.10 fails to run doctests for some reason\n  if impl(ghc >= 8.10) && impl(ghc < 8.11)\n    buildable: False\n  build-depends:\n        base      -any\n      , doctest   >=0.15 && <0.18\n      , primitive >= 0.6.4.0 && < 0.8\n      , vector    -any\n";
    }