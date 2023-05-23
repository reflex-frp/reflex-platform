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
      identifier = { name = "test-framework"; version = "0.8.2.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Libraries List <libraries@haskell.org>";
      author = "Max Bolingbroke <batterseapower@hotmail.com>";
      homepage = "http://haskell.github.io/test-framework/";
      url = "";
      synopsis = "Framework for running and organising tests, with HUnit and QuickCheck support";
      description = "Allows tests such as QuickCheck properties and HUnit test cases to be assembled into test groups, run in\nparallel (but reported in deterministic order, to aid diff interpretation) and filtered and controlled by\ncommand line options. All of this comes with colored test output, progress reporting and test statistics output.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
          (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."regex-posix" or (errorHandler.buildDepError "regex-posix"))
          (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."xml" or (errorHandler.buildDepError "xml"))
          (hsPkgs."hostname" or (errorHandler.buildDepError "hostname"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.8")) (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      tests = {
        "test-framework-tests" = {
          depends = [
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
            (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
            (hsPkgs."regex-posix" or (errorHandler.buildDepError "regex-posix"))
            (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."xml" or (errorHandler.buildDepError "xml"))
            (hsPkgs."hostname" or (errorHandler.buildDepError "hostname"))
            (hsPkgs."libxml" or (errorHandler.buildDepError "libxml"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/test-framework-0.8.2.0.tar.gz";
      sha256 = "f5aec7a15dbcb39e951bcf6502606fd99d751197b5510f41706899aa7e660ac2";
      });
    }) // {
    package-description-override = "cabal-version:       >= 1.10\r\nName:                test-framework\r\nVersion:             0.8.2.0\r\nx-revision: 6\r\n\r\nBuild-Type:          Simple\r\nCategory:            Testing\r\nSynopsis:            Framework for running and organising tests, with HUnit and QuickCheck support\r\nDescription:         Allows tests such as QuickCheck properties and HUnit test cases to be assembled into test groups, run in\r\n                     parallel (but reported in deterministic order, to aid diff interpretation) and filtered and controlled by\r\n                     command line options. All of this comes with colored test output, progress reporting and test statistics output.\r\nLicense:             BSD3\r\nLicense-File:        LICENSE\r\nAuthor:              Max Bolingbroke <batterseapower@hotmail.com>\r\nMaintainer:          Libraries List <libraries@haskell.org>\r\nHomepage:            http://haskell.github.io/test-framework/\r\nBug-Reports:         https://github.com/haskell/test-framework/issues\r\nTested-With:         GHC==8.2.2, GHC==8.0.2, GHC==7.10.3, GHC==7.8.4, GHC==7.6.3, GHC==7.4.2, GHC==7.2.2, GHC==7.0.4\r\n\r\nExtra-Source-Files: ChangeLog.md\r\n\r\nLibrary\r\n        Exposed-Modules:        Test.Framework\r\n                                Test.Framework.Options\r\n                                Test.Framework.Providers.API\r\n                                Test.Framework.Runners.Console\r\n                                Test.Framework.Runners.Options\r\n                                Test.Framework.Runners.TestPattern\r\n                                Test.Framework.Runners.API\r\n                                Test.Framework.Seed\r\n\r\n        Other-Modules:          Test.Framework.Core\r\n                                Test.Framework.Improving\r\n                                Test.Framework.Runners.Console.Colors\r\n                                Test.Framework.Runners.Console.ProgressBar\r\n                                Test.Framework.Runners.Console.Run\r\n                                Test.Framework.Runners.Console.Statistics\r\n                                Test.Framework.Runners.Console.Table\r\n                                Test.Framework.Runners.Console.Utilities\r\n                                Test.Framework.Runners.Core\r\n                                Test.Framework.Runners.Processors\r\n                                Test.Framework.Runners.Statistics\r\n                                Test.Framework.Runners.ThreadPool\r\n                                Test.Framework.Runners.TimedConsumption\r\n                                Test.Framework.Runners.XML.JUnitWriter\r\n                                Test.Framework.Runners.XML\r\n                                Test.Framework.Utilities\r\n\r\n        Build-Depends:          base           >= 4.3    && < 5\r\n                              , ansi-terminal  >= 0.4.0  && < 0.12\r\n                              , ansi-wl-pprint >= 0.5.1  && < 0.7\r\n                              , random         >= 1.0    && < 1.3\r\n                              , containers     >= 0.1    && < 0.7\r\n                              , regex-posix    >= 0.72   && < 0.97\r\n                              , old-locale     >= 1.0    && < 1.1\r\n                              , time           >= 1.1.2  && < 1.12\r\n                              , xml            >= 1.3.5  && < 1.4\r\n                              , hostname       >= 1.0    && < 1.1\r\n\r\n        if !impl(ghc >= 7.8)\r\n                Build-Depends:  base-orphans   >= 0.1    && < 0.9\r\n\r\n        if !impl(ghc >= 8.0)\r\n                Build-Depends:  semigroups     >= 0.18   && < 0.20\r\n\r\n        Default-Language:       Haskell2010\r\n        Default-Extensions:     CPP\r\n                                PatternGuards\r\n                                ExistentialQuantification\r\n                                RecursiveDo\r\n                                FlexibleInstances\r\n                                TypeSynonymInstances\r\n                                TypeOperators\r\n                                FunctionalDependencies\r\n                                MultiParamTypeClasses\r\n\r\n        -- workaround https://github.com/haskell/cabal/issues/4443\r\n        if impl(ghc >= 7.2)\r\n                Default-Extensions: NondecreasingIndentation\r\n        Ghc-Options:            -Wall\r\n\r\n        if impl(ghc)\r\n                Cpp-Options:            -DCOMPILER_GHC\r\n\r\n        if impl(ghc >= 8.0)\r\n                Ghc-Options: -Wcompat -Wnoncanonical-monad-instances -Wnoncanonical-monadfail-instances\r\n\r\nTest-Suite test-framework-tests\r\n        Main-Is:                Test/Framework/Tests.hs\r\n        Type:                   exitcode-stdio-1.0\r\n\r\n        -- Buildable:              False\r\n        Build-Depends:          HUnit          >= 1.2\r\n                              , QuickCheck     >= 2.3 && < 2.15\r\n                              , base           >= 4.3\r\n                              , random         >= 1.0\r\n                              , containers     >= 0.1\r\n                              , ansi-terminal  >= 0.4.0\r\n                              , ansi-wl-pprint >= 0.5.1\r\n                              , regex-posix    >= 0.72\r\n                              , old-locale     >= 1.0\r\n                              , time           >= 1.1.2\r\n                              , xml            >= 1.3.5\r\n                              , hostname       >= 1.0\r\n                              , libxml         >= 0.1.1\r\n                              , bytestring     >= 0.9\r\n                              , semigroups     >= 0.18\r\n\r\n        Default-Language:       Haskell2010\r\n        Default-Extensions:     CPP\r\n                                PatternGuards\r\n                                ExistentialQuantification\r\n                                RecursiveDo\r\n                                FlexibleInstances\r\n                                TypeSynonymInstances\r\n                                TypeOperators\r\n                                FunctionalDependencies\r\n                                MultiParamTypeClasses\r\n\r\n        if impl(ghc >= 7.2)\r\n            Default-Extensions: NondecreasingIndentation\r\n\r\n        Cpp-Options:            -DTEST\r\n\r\n        Ghc-Options:            -Wall -threaded\r\n\r\n        if impl(ghc)\r\n                Cpp-Options:            -DCOMPILER_GHC\r\n\r\nSource-Repository head\r\n  Type:     git\r\n  Location: https://github.com/haskell/test-framework.git\r\n  subdir:   core\r\n";
    }