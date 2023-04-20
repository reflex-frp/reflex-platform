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
      debug-expensive-assertions = false;
      debug-conflict-sets = false;
      debug-tracetree = false;
      };
    package = {
      specVersion = "2.2";
      identifier = { name = "cabal-install-solver"; version = "3.8.1.0"; };
      license = "BSD-3-Clause";
      copyright = "2003-2022, Cabal Development Team";
      maintainer = "Cabal Development Team <cabal-devel@haskell.org>";
      author = "Cabal Development Team (see AUTHORS file)";
      homepage = "http://www.haskell.org/cabal/";
      url = "";
      synopsis = "The command-line interface for Cabal and Hackage.";
      description = "The solver component used in cabal-install command-line program";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
          (hsPkgs."Cabal-syntax" or (errorHandler.buildDepError "Cabal-syntax"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."edit-distance" or (errorHandler.buildDepError "edit-distance"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ] ++ (pkgs.lib).optional (flags.debug-conflict-sets) (hsPkgs."base" or (errorHandler.buildDepError "base"))) ++ (pkgs.lib).optional (flags.debug-tracetree) (hsPkgs."tracetree" or (errorHandler.buildDepError "tracetree"));
        buildable = true;
        };
      tests = {
        "unit-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."Cabal-syntax" or (errorHandler.buildDepError "Cabal-syntax"))
            (hsPkgs."cabal-install-solver" or (errorHandler.buildDepError "cabal-install-solver"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/cabal-install-solver-3.8.1.0.tar.gz";
      sha256 = "df2369f6c37517a3b2625bc19057d9e206bbb40386bcb607f17dc7d2e588ffe7";
      });
    }) // {
    package-description-override = "cabal-version: 2.2\r\nname:          cabal-install-solver\r\nversion:       3.8.1.0\r\nx-revision: 1\r\nsynopsis:      The command-line interface for Cabal and Hackage.\r\ndescription:\r\n  The solver component used in cabal-install command-line program\r\n\r\nhomepage:      http://www.haskell.org/cabal/\r\nbug-reports:   https://github.com/haskell/cabal/issues\r\nlicense:       BSD-3-Clause\r\nlicense-file:  LICENSE\r\nauthor:        Cabal Development Team (see AUTHORS file)\r\nmaintainer:    Cabal Development Team <cabal-devel@haskell.org>\r\ncopyright:     2003-2022, Cabal Development Team\r\ncategory:      Distribution\r\nbuild-type:    Simple\r\nExtra-Source-Files:\r\n  ChangeLog.md\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/haskell/cabal/\r\n  subdir:   cabal-install-solver\r\n\r\nflag debug-expensive-assertions\r\n  description: Enable expensive assertions for testing or debugging\r\n  default:     False\r\n  manual:      True\r\n\r\nflag debug-conflict-sets\r\n  description: Add additional information to ConflictSets\r\n  default:     False\r\n  manual:      True\r\n\r\nflag debug-tracetree\r\n  description: Compile in support for tracetree (used to debug the solver)\r\n  default:     False\r\n  manual:      True\r\n\r\nlibrary\r\n  default-language: Haskell2010\r\n  hs-source-dirs:   src\r\n  hs-source-dirs:   src-assertion\r\n  ghc-options:\r\n    -Wall -Wcompat -Wnoncanonical-monad-instances\r\n    -fwarn-tabs -fwarn-incomplete-uni-patterns\r\n\r\n  if impl(ghc <8.8)\r\n    ghc-options: -Wnoncanonical-monadfail-instances\r\n  if impl(ghc >=8.10)\r\n    ghc-options: -Wunused-packages\r\n\r\n  exposed-modules:\r\n    Distribution.Client.Utils.Assertion\r\n\r\n    Distribution.Solver.Compat.Prelude\r\n    Distribution.Solver.Modular\r\n    Distribution.Solver.Modular.Assignment\r\n    Distribution.Solver.Modular.Builder\r\n    Distribution.Solver.Modular.Configured\r\n    Distribution.Solver.Modular.ConfiguredConversion\r\n    Distribution.Solver.Modular.ConflictSet\r\n    Distribution.Solver.Modular.Cycles\r\n    Distribution.Solver.Modular.Dependency\r\n    Distribution.Solver.Modular.Explore\r\n    Distribution.Solver.Modular.Flag\r\n    Distribution.Solver.Modular.Index\r\n    Distribution.Solver.Modular.IndexConversion\r\n    Distribution.Solver.Modular.LabeledGraph\r\n    Distribution.Solver.Modular.Linking\r\n    Distribution.Solver.Modular.Log\r\n    Distribution.Solver.Modular.Message\r\n    Distribution.Solver.Modular.MessageUtils\r\n    Distribution.Solver.Modular.Package\r\n    Distribution.Solver.Modular.Preference\r\n    Distribution.Solver.Modular.PSQ\r\n    Distribution.Solver.Modular.RetryLog\r\n    Distribution.Solver.Modular.Solver\r\n    Distribution.Solver.Modular.Tree\r\n    Distribution.Solver.Modular.Validate\r\n    Distribution.Solver.Modular.Var\r\n    Distribution.Solver.Modular.Version\r\n    Distribution.Solver.Modular.WeightedPSQ\r\n    Distribution.Solver.Types.ComponentDeps\r\n    Distribution.Solver.Types.ConstraintSource\r\n    Distribution.Solver.Types.DependencyResolver\r\n    Distribution.Solver.Types.Flag\r\n    Distribution.Solver.Types.InstalledPreference\r\n    Distribution.Solver.Types.InstSolverPackage\r\n    Distribution.Solver.Types.LabeledPackageConstraint\r\n    Distribution.Solver.Types.OptionalStanza\r\n    Distribution.Solver.Types.PackageConstraint\r\n    Distribution.Solver.Types.PackageFixedDeps\r\n    Distribution.Solver.Types.PackageIndex\r\n    Distribution.Solver.Types.PackagePath\r\n    Distribution.Solver.Types.PackagePreferences\r\n    Distribution.Solver.Types.PkgConfigDb\r\n    Distribution.Solver.Types.Progress\r\n    Distribution.Solver.Types.ResolverPackage\r\n    Distribution.Solver.Types.Settings\r\n    Distribution.Solver.Types.SolverId\r\n    Distribution.Solver.Types.SolverPackage\r\n    Distribution.Solver.Types.SourcePackage\r\n    Distribution.Solver.Types.Variable\r\n\r\n  build-depends:\r\n    , array         >=0.4      && <0.6\r\n    , base          >=4.10     && <4.18\r\n    , bytestring    >=0.10.6.0 && <0.12\r\n    , Cabal         ^>=3.8\r\n    , Cabal-syntax  ^>=3.8\r\n    , containers    >=0.5.6.2  && <0.7\r\n    , edit-distance ^>= 0.2.2\r\n    , filepath      ^>=1.4.0.0\r\n    , mtl           >=2.0      && <2.3\r\n    , pretty        ^>=1.1\r\n    , transformers  >=0.4.2.0  && <0.6\r\n\r\n  if flag(debug-expensive-assertions)\r\n    cpp-options: -DDEBUG_EXPENSIVE_ASSERTIONS\r\n\r\n  if flag(debug-conflict-sets)\r\n    cpp-options:   -DDEBUG_CONFLICT_SETS\r\n    build-depends: base >=4.8\r\n\r\n  if flag(debug-tracetree)\r\n    cpp-options:   -DDEBUG_TRACETREE\r\n    build-depends: tracetree ^>=0.1\r\n\r\nTest-Suite unit-tests\r\n   default-language: Haskell2010\r\n   ghc-options: -rtsopts -threaded\r\n\r\n   type: exitcode-stdio-1.0\r\n   main-is: UnitTests.hs\r\n   hs-source-dirs: tests\r\n   other-modules:\r\n     UnitTests.Distribution.Solver.Modular.MessageUtils\r\n\r\n   build-depends:\r\n     , base        >= 4.10  && <4.18\r\n     , Cabal\r\n     , Cabal-syntax\r\n     , cabal-install-solver\r\n     , tasty       >= 1.2.3 && <1.5\r\n     , tasty-quickcheck\r\n     , tasty-hunit >= 0.10\r\n";
    }