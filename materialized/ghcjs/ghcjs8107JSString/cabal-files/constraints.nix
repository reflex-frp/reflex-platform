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
      identifier = { name = "constraints"; version = "0.13"; };
      license = "BSD-2-Clause";
      copyright = "Copyright (C) 2011-2015 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/constraints/";
      url = "";
      synopsis = "Constraint manipulation";
      description = "GHC 7.4 gave us the ability to talk about @ConstraintKinds@. They stopped crashing the compiler in GHC 7.6.\n\nThis package provides a vocabulary for working with them.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          (hsPkgs."type-equality" or (errorHandler.buildDepError "type-equality"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."constraints" or (errorHandler.buildDepError "constraints"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/constraints-0.13.tar.gz";
      sha256 = "9259af54682f2673931978d96074c147406b1e18bd9111903fcaefe9252a6590";
      });
    }) // {
    package-description-override = "name:          constraints\ncategory:      Constraints\nversion:       0.13\nlicense:       BSD2\ncabal-version: >= 1.10\nlicense-file:  LICENSE\nauthor:        Edward A. Kmett\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\nstability:     experimental\nhomepage:      http://github.com/ekmett/constraints/\nbug-reports:   http://github.com/ekmett/constraints/issues\ncopyright:     Copyright (C) 2011-2015 Edward A. Kmett\nsynopsis:      Constraint manipulation\ndescription:\n  GHC 7.4 gave us the ability to talk about @ConstraintKinds@. They stopped crashing the compiler in GHC 7.6.\n  .\n  This package provides a vocabulary for working with them.\n\nbuild-type:    Simple\ntested-with:   GHC == 7.8.4\n             , GHC == 7.10.3\n             , GHC == 8.0.2\n             , GHC == 8.2.2\n             , GHC == 8.4.4\n             , GHC == 8.6.5\n             , GHC == 8.8.3\n             , GHC == 8.10.1\nextra-source-files: README.markdown\n                  , CHANGELOG.markdown\n\nsource-repository head\n  type: git\n  location: git://github.com/ekmett/constraints.git\n\nlibrary\n  hs-source-dirs: src\n\n  default-language: Haskell2010\n  other-extensions:\n    FunctionalDependencies,\n    ScopedTypeVariables,\n    StandaloneDeriving,\n    FlexibleInstances,\n    FlexibleContexts,\n    ConstraintKinds,\n    KindSignatures,\n    TypeOperators,\n    Rank2Types,\n    GADTs\n\n  build-depends:\n    base >= 4.7 && < 5,\n    binary >= 0.7.1 && < 0.9,\n    deepseq >= 1.3 && < 1.5,\n    ghc-prim,\n    hashable >= 1.2 && < 1.4,\n    mtl >= 2.1.2 && < 2.3,\n    transformers >= 0.3.0.0 && < 0.6,\n    transformers-compat >= 0.5 && < 1,\n    type-equality >= 1 && < 2\n  if impl(ghc < 8.0)\n    build-depends: semigroups >= 0.17 && < 0.20\n\n  exposed-modules:\n    Data.Constraint\n    Data.Constraint.Deferrable\n    Data.Constraint.Forall\n    Data.Constraint.Lifting\n    Data.Constraint.Unsafe\n\n  if impl(ghc >= 8)\n    exposed-modules:\n      Data.Constraint.Nat\n      Data.Constraint.Symbol\n\n  ghc-options: -Wall\n  if impl(ghc >= 8.6)\n    ghc-options: -Wno-star-is-type\n\ntest-suite spec\n  type: exitcode-stdio-1.0\n  default-language: Haskell2010\n  hs-source-dirs: tests\n  main-is: Spec.hs\n  other-modules: GH55Spec\n  ghc-options: -Wall -threaded -rtsopts\n  build-tool-depends: hspec-discover:hspec-discover >= 2\n  build-depends:\n    base >= 4.7 && < 5,\n    constraints,\n    hspec >= 2\n";
    }