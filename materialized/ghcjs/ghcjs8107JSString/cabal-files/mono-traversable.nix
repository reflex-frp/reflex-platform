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
      identifier = { name = "mono-traversable"; version = "1.0.15.1"; };
      license = "MIT";
      copyright = "";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman, John Wiegley, Greg Weber";
      homepage = "https://github.com/snoyberg/mono-traversable#readme";
      url = "";
      synopsis = "Type classes for mapping, folding, and traversing monomorphic containers";
      description = "Please see the README at <https://www.stackage.org/package/mono-traversable>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."split" or (errorHandler.buildDepError "split"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."vector-algorithms" or (errorHandler.buildDepError "vector-algorithms"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."foldl" or (errorHandler.buildDepError "foldl"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."mono-traversable" or (errorHandler.buildDepError "mono-traversable"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "sorting" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."mono-traversable" or (errorHandler.buildDepError "mono-traversable"))
            (hsPkgs."mwc-random" or (errorHandler.buildDepError "mwc-random"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/mono-traversable-1.0.15.1.tar.gz";
      sha256 = "c2df5b79ed2f88f2ee313e57c1d591d4463788e20d39e439297eec5ba5835ddf";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\n\n-- This file has been generated from package.yaml by hpack version 0.31.2.\n--\n-- see: https://github.com/sol/hpack\n--\n-- hash: b2ac08c2845dd12213a3bc3c6e01f805bb98a7693a588b0ae313ceadcb5ca592\n\nname:           mono-traversable\nversion:        1.0.15.1\nsynopsis:       Type classes for mapping, folding, and traversing monomorphic containers\ndescription:    Please see the README at <https://www.stackage.org/package/mono-traversable>\ncategory:       Data\nhomepage:       https://github.com/snoyberg/mono-traversable#readme\nbug-reports:    https://github.com/snoyberg/mono-traversable/issues\nauthor:         Michael Snoyman, John Wiegley, Greg Weber\nmaintainer:     michael@snoyman.com\nlicense:        MIT\nlicense-file:   LICENSE\nbuild-type:     Simple\nextra-source-files:\n    README.md\n    ChangeLog.md\n\nsource-repository head\n  type: git\n  location: https://github.com/snoyberg/mono-traversable\n\nlibrary\n  exposed-modules:\n      Data.Containers\n      Data.MonoTraversable\n      Data.MonoTraversable.Unprefixed\n      Data.NonNull\n      Data.Sequences\n  other-modules:\n      Paths_mono_traversable\n  hs-source-dirs:\n      src\n  ghc-options: -Wall\n  build-depends:\n      base >=4.10 && <5\n    , bytestring >=0.9\n    , containers >=0.5.8\n    , hashable\n    , split >=0.2\n    , text >=0.11\n    , transformers >=0.3\n    , unordered-containers >=0.2\n    , vector >=0.10\n    , vector-algorithms >=0.6\n  if impl(ghc <8.0)\n    build-depends:\n        semigroups >=0.10\n  default-language: Haskell2010\n\ntest-suite test\n  type: exitcode-stdio-1.0\n  main-is: main.hs\n  other-modules:\n      Spec\n      Paths_mono_traversable\n  hs-source-dirs:\n      test\n  ghc-options: -O0\n  build-depends:\n      HUnit\n    , QuickCheck\n    , base\n    , bytestring\n    , containers\n    , foldl\n    , hspec\n    , mono-traversable\n    , semigroups\n    , text\n    , transformers\n    , unordered-containers\n    , vector\n  default-language: Haskell2010\n\nbenchmark sorting\n  type: exitcode-stdio-1.0\n  main-is: sorting.hs\n  other-modules:\n      Paths_mono_traversable\n  hs-source-dirs:\n      bench\n  ghc-options: -Wall -O2\n  build-depends:\n      base\n    , gauge\n    , mono-traversable\n    , mwc-random\n    , vector\n  default-language: Haskell2010\n";
    }