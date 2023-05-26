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
      specVersion = "1.8";
      identifier = { name = "lifted-base"; version = "0.2.3.12"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2011-2012 Bas van Dijk, Anders Kaseorg";
      maintainer = "Bas van Dijk <v.dijk.bas@gmail.com>";
      author = "Bas van Dijk, Anders Kaseorg";
      homepage = "https://github.com/basvandijk/lifted-base";
      url = "";
      synopsis = "lifted IO operations from the base library";
      description = "@lifted-base@ exports IO operations from the base library lifted to\nany instance of 'MonadBase' or 'MonadBaseControl'.\n\nNote that not all modules from @base@ are converted yet. If\nyou need a lifted version of a function from @base@, just\nask me to add it or send me a patch.\n\nThe package includes a copy of the @monad-peel@ testsuite written\nby Anders Kaseorg The tests can be performed using @cabal test@.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          ];
        buildable = true;
        };
      tests = {
        "test-lifted-base" = {
          depends = [
            (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench-lifted-base" = {
          depends = [
            (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."monad-peel" or (errorHandler.buildDepError "monad-peel"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/lifted-base-0.2.3.12.tar.gz";
      sha256 = "c134a95f56750aae806e38957bb03c59627cda16034af9e00a02b699474317c5";
      });
    }) // {
    package-description-override = "Name:                lifted-base\nVersion:             0.2.3.12\nSynopsis:            lifted IO operations from the base library\nLicense:             BSD3\nLicense-file:        LICENSE\nAuthor:              Bas van Dijk, Anders Kaseorg\nMaintainer:          Bas van Dijk <v.dijk.bas@gmail.com>\nCopyright:           (c) 2011-2012 Bas van Dijk, Anders Kaseorg\nHomepage:            https://github.com/basvandijk/lifted-base\nBug-reports:         https://github.com/basvandijk/lifted-base/issues\nCategory:            Control\nBuild-type:          Simple\nCabal-version:       >= 1.8\nDescription:         @lifted-base@ exports IO operations from the base library lifted to\n                     any instance of 'MonadBase' or 'MonadBaseControl'.\n                     .\n                     Note that not all modules from @base@ are converted yet. If\n                     you need a lifted version of a function from @base@, just\n                     ask me to add it or send me a patch.\n                     .\n                     The package includes a copy of the @monad-peel@ testsuite written\n                     by Anders Kaseorg The tests can be performed using @cabal test@.\n\nextra-source-files:  README.markdown, NEWS\n\nextra-source-files: include/inlinable.h\n\n--------------------------------------------------------------------------------\n\nsource-repository head\n  type:     git\n  location: https://github.com/basvandijk/lifted-base.git\n\n--------------------------------------------------------------------------------\n\nLibrary\n  Exposed-modules: Control.Exception.Lifted\n                   Control.Concurrent.MVar.Lifted\n                   Control.Concurrent.Chan.Lifted\n                   Control.Concurrent.QSem.Lifted\n                   Control.Concurrent.QSemN.Lifted\n                   Control.Concurrent.Lifted\n                   Data.IORef.Lifted\n                   Foreign.Marshal.Utils.Lifted\n                   System.Timeout.Lifted\n  if impl(ghc < 7.8)\n    Exposed-modules:\n                   Control.Concurrent.SampleVar.Lifted\n\n  Build-depends: base              >= 3 && < 5\n               , transformers-base >= 0.4\n               , monad-control     >= 0.3\n\n  Include-dirs: include\n  Includes:     inlinable.h\n\n  Ghc-options: -Wall\n\n--------------------------------------------------------------------------------\n\ntest-suite test-lifted-base\n  type:           exitcode-stdio-1.0\n  main-is:        test.hs\n  hs-source-dirs: test\n\n  build-depends: lifted-base\n               , base                 >= 3 && < 5\n               , transformers         >= 0.3\n               , transformers-base    >= 0.4.4\n               , transformers-compat  >= 0.3\n               , monad-control        >= 1.0.0.3\n               , HUnit                >= 1.2.2\n               , test-framework       >= 0.2.4\n               , test-framework-hunit >= 0.2.4\n\n  Include-dirs: include\n  Includes:     inlinable.h\n\n  ghc-options: -Wall\n\n--------------------------------------------------------------------------------\n\nbenchmark bench-lifted-base\n  type:           exitcode-stdio-1.0\n  main-is:        bench.hs\n  hs-source-dirs: bench\n\n  ghc-options:    -O2\n\n  build-depends: lifted-base\n               , base          >= 3 && < 5\n               , transformers  >= 0.2\n               , criterion     >= 1\n               , monad-control >= 0.3\n               , monad-peel    >= 0.1\n";
    }