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
      bench = true;
      properties = true;
      llvm = false;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "vector-algorithms"; version = "0.8.0.4"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2008,2009,2010,2011,2012,2013,2014,2015 Dan Doel\n(c) 2015 Tim Baumann";
      maintainer = "Dan Doel <dan.doel@gmail.com>\nErik de Castro Lopo <erikd@mega-nerd.com>";
      author = "Dan Doel";
      homepage = "https://github.com/erikd/vector-algorithms/";
      url = "";
      synopsis = "Efficient algorithms for vector arrays";
      description = "Efficient algorithms for sorting vector arrays. At some stage\nother vector algorithms may be added.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.8")) (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"));
        buildable = true;
        };
      tests = {
        "properties" = {
          depends = (pkgs.lib).optionals (!(!flags.properties)) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."vector-algorithms" or (errorHandler.buildDepError "vector-algorithms"))
            ];
          buildable = if !flags.properties then false else true;
          };
        };
      benchmarks = {
        "simple-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."mwc-random" or (errorHandler.buildDepError "mwc-random"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."vector-algorithms" or (errorHandler.buildDepError "vector-algorithms"))
            ];
          buildable = if !flags.bench then false else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/vector-algorithms-0.8.0.4.tar.gz";
      sha256 = "76176a56778bf30a275b1089ee6db24ec6c67d92525145f8dfe215b80137af3b";
      });
    }) // {
    package-description-override = "name:              vector-algorithms\r\nversion:           0.8.0.4\r\nx-revision: 1\r\nlicense:           BSD3\r\nlicense-file:      LICENSE\r\nauthor:            Dan Doel\r\nmaintainer:        Dan Doel <dan.doel@gmail.com>\r\n                   Erik de Castro Lopo <erikd@mega-nerd.com>\r\ncopyright:         (c) 2008,2009,2010,2011,2012,2013,2014,2015 Dan Doel\r\n                   (c) 2015 Tim Baumann\r\nhomepage:          https://github.com/erikd/vector-algorithms/\r\ncategory:          Data\r\nsynopsis:          Efficient algorithms for vector arrays\r\ndescription:       Efficient algorithms for sorting vector arrays. At some stage\r\n                   other vector algorithms may be added.\r\nbuild-type:        Simple\r\ncabal-version:     >= 1.10\r\nextra-source-files: CHANGELOG.md\r\n\r\n\r\nflag BoundsChecks\r\n  description: Enable bounds checking\r\n  default: True\r\n\r\nflag UnsafeChecks\r\n  description: Enable bounds checking in unsafe operations at the cost of a\r\n               significant performance penalty.\r\n  default: False\r\n\r\nflag InternalChecks\r\n  description: Enable internal consistency checks at the cost of a\r\n               significant performance penalty.\r\n  default: False\r\n\r\nflag bench\r\n  description: Build a benchmarking program to test vector-algorithms\r\n               performance\r\n  default: True\r\n\r\nflag properties\r\n  description: Enable the quickcheck tests\r\n  default: True\r\n\r\n-- flag dump-simpl\r\n--   description: Dumps the simplified core during compilation\r\n--   default: False\r\n\r\nflag llvm\r\n  description: Build using llvm\r\n  default: False\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/erikd/vector-algorithms/\r\n\r\nlibrary\r\n  hs-source-dirs: src\r\n  default-language: Haskell2010\r\n\r\n  build-depends: base >= 4.5 && < 5,\r\n                 vector >= 0.6 && < 0.14,\r\n                 primitive >=0.3 && <0.8,\r\n                 bytestring >= 0.9 && < 1.0\r\n\r\n  if ! impl (ghc >= 7.8)\r\n    build-depends: tagged >= 0.4 && < 0.9\r\n\r\n  exposed-modules:\r\n    Data.Vector.Algorithms.Optimal\r\n    Data.Vector.Algorithms.Insertion\r\n    Data.Vector.Algorithms.Intro\r\n    Data.Vector.Algorithms.Merge\r\n    Data.Vector.Algorithms.Radix\r\n    Data.Vector.Algorithms.Search\r\n    Data.Vector.Algorithms.Heap\r\n    Data.Vector.Algorithms.AmericanFlag\r\n    Data.Vector.Algorithms.Tim\r\n\r\n  other-modules:\r\n    Data.Vector.Algorithms.Common\r\n\r\n  ghc-options:\r\n    -funbox-strict-fields\r\n\r\n  -- Cabal/Hackage complains about these\r\n  -- if flag(dump-simpl)\r\n  --   ghc-options: -ddump-simpl -ddump-to-file\r\n\r\n  if flag(llvm)\r\n    ghc-options: -fllvm\r\n\r\n  include-dirs:\r\n    include\r\n\r\n  install-includes:\r\n    vector.h\r\n\r\n  if flag(BoundsChecks)\r\n    cpp-options: -DVECTOR_BOUNDS_CHECKS\r\n\r\n  if flag(UnsafeChecks)\r\n    cpp-options: -DVECTOR_UNSAFE_CHECKS\r\n\r\n  if flag(InternalChecks)\r\n    cpp-options: -DVECTOR_INTERNAL_CHECKS\r\n\r\nbenchmark simple-bench\r\n  hs-source-dirs: bench/simple\r\n  type: exitcode-stdio-1.0\r\n  default-language: Haskell2010\r\n\r\n  if !flag(bench)\r\n    buildable: False\r\n\r\n  main-is: Main.hs\r\n\r\n  other-modules:\r\n    Blocks\r\n\r\n  build-depends: base, mwc-random, vector, vector-algorithms\r\n  ghc-options: -Wall\r\n\r\n  -- Cabal/Hackage complains about these\r\n  -- if flag(dump-simpl)\r\n  --   ghc-options: -ddump-simpl -ddump-to-file\r\n\r\n  if flag(llvm)\r\n    ghc-options: -fllvm\r\n\r\ntest-suite properties\r\n  hs-source-dirs: tests/properties\r\n  type: exitcode-stdio-1.0\r\n  main-is: Tests.hs\r\n  default-language: Haskell2010\r\n\r\n  other-modules:\r\n    Optimal\r\n    Properties\r\n    Util\r\n\r\n  if !flag(properties)\r\n    buildable: False\r\n  else\r\n    build-depends:\r\n      base,\r\n      bytestring,\r\n      containers,\r\n      QuickCheck > 2.9 && < 2.15,\r\n      vector,\r\n      vector-algorithms\r\n\r\n  if flag(llvm)\r\n    ghc-options: -fllvm\r\n\r\n";
    }