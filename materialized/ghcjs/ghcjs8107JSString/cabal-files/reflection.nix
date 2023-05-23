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
    flags = { slow = false; template-haskell = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "reflection"; version = "2.1.6"; };
      license = "BSD-3-Clause";
      copyright = "2009-2013 Edward A. Kmett,\n2012 Elliott Hird,\n2004 Oleg Kiselyov and Chung-chieh Shan";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett, Elliott Hird, Oleg Kiselyov and Chung-chieh Shan";
      homepage = "http://github.com/ekmett/reflection";
      url = "";
      synopsis = "Reifies arbitrary terms into types that can be reflected back into terms";
      description = "This package addresses the /configuration problem/ which is\npropagating configurations that are available at run-time, allowing\nmultiple configurations to coexist without resorting to mutable\nglobal variables or 'System.IO.Unsafe.unsafePerformIO'.\n\nThat package is an implementation of the ideas presented in the\npaper \\\"Functional Pearl: Implicit Configurations\\\" by Oleg Kiselyov\nand Chung-chieh Shan (<http://okmij.org/ftp/Haskell/tr-15-04.pdf original paper>).\nHowever, the API has been streamlined to improve performance.\n\nAustin Seipp's tutorial <https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection Reflecting values to types and back> provides a summary of the\napproach taken by this library, along with more motivating examples.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.8") (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optional (flags.template-haskell && (compiler.isGhc && true)) (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"));
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."reflection" or (errorHandler.buildDepError "reflection"))
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
      url = "http://hackage.haskell.org/package/reflection-2.1.6.tar.gz";
      sha256 = "bf3e14917ebb329a53701a3cce0afe670f20037a0148dbfa5cbfa574ed6ba6cd";
      });
    }) // {
    package-description-override = "name:           reflection\nversion:        2.1.6\nlicense:        BSD3\nlicense-file:   LICENSE\nauthor:         Edward A. Kmett, Elliott Hird, Oleg Kiselyov and Chung-chieh Shan\nmaintainer:     Edward A. Kmett <ekmett@gmail.com>\nstability:      experimental\nhomepage:       http://github.com/ekmett/reflection\nbug-reports:    http://github.com/ekmett/reflection/issues\ncategory:       Data, Reflection, Dependent Types\nsynopsis:       Reifies arbitrary terms into types that can be reflected back into terms\ncopyright:      2009-2013 Edward A. Kmett,\n                2012 Elliott Hird,\n                2004 Oleg Kiselyov and Chung-chieh Shan\nbuild-type:     Simple\ncabal-version:  >= 1.10\ndescription:\n  This package addresses the /configuration problem/ which is\n  propagating configurations that are available at run-time, allowing\n  multiple configurations to coexist without resorting to mutable\n  global variables or 'System.IO.Unsafe.unsafePerformIO'.\n  .\n  That package is an implementation of the ideas presented in the\n  paper \\\"Functional Pearl: Implicit Configurations\\\" by Oleg Kiselyov\n  and Chung-chieh Shan (<http://okmij.org/ftp/Haskell/tr-15-04.pdf original paper>).\n  However, the API has been streamlined to improve performance.\n  .\n  Austin Seipp's tutorial <https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection Reflecting values to types and back> provides a summary of the\n  approach taken by this library, along with more motivating examples.\ntested-with:   GHC == 7.0.4\n             , GHC == 7.2.2\n             , GHC == 7.4.2\n             , GHC == 7.6.3\n             , GHC == 7.8.4\n             , GHC == 7.10.3\n             , GHC == 8.0.2\n             , GHC == 8.2.2\n             , GHC == 8.4.4\n             , GHC == 8.6.5\n             , GHC == 8.8.3\n             , GHC == 8.10.1\n\nextra-source-files:\n  examples/reflection-examples.cabal\n  examples/LICENSE\n  examples/*.hs\n  CHANGELOG.markdown\n  README.markdown\n  slow/Data/Reflection.hs\n  fast/Data/Reflection.hs\n  .travis.yml\n\nflag slow\n  description:\n    If you enable this flag, we use a more portable much much slower implementation. Moreover, the 'Given' API is broken, so this is currently an unsupported configuration. If you feel the need to turn on this flag for any reason, please email the maintainer!\n  default: False\n  manual: False\n\nflag template-haskell\n  description:\n    You can disable the use of the `template-haskell` package using `-f-template-haskell`.\n    .\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\n  default: True\n  manual: True\n\nsource-repository head\n  type: git\n  location: git://github.com/ekmett/reflection.git\n\nlibrary\n  ghc-options: -Wall\n\n  if impl(ghc >= 7.2)\n    default-extensions: Trustworthy\n\n  build-depends:\n    base >= 2 && < 5\n\n  if impl(ghc < 7.8)\n    build-depends:\n      tagged >= 0.4.4 && < 1\n\n  if !impl(ghc >= 8.0)\n    build-depends:\n      semigroups >= 0.11 && < 0.20\n\n  default-language: Haskell98\n\n  if flag(template-haskell) && impl(ghc)\n    if !impl(ghc >= 8.0)\n      other-extensions: TemplateHaskell\n    -- else\n    --   other-extensions: TemplateHaskellQuotes -- Hackage doesn't know this extension yet\n    build-depends: template-haskell\n\n  if !flag(slow) && (impl(ghc) || impl(hugs))\n    hs-source-dirs: fast\n  else\n    other-extensions: ScopedTypeVariables, FlexibleInstances\n    hs-source-dirs: slow\n\n  other-extensions:\n    MultiParamTypeClasses,\n    FunctionalDependencies,\n    Rank2Types,\n    CPP\n\n  exposed-modules: Data.Reflection\n\ntest-suite spec\n  type: exitcode-stdio-1.0\n  hs-source-dirs: tests\n  main-is: Spec.hs\n  other-modules: ReifyNatSpec\n                 T47Spec\n  ghc-options: -Wall\n  default-language: Haskell98\n  build-tool-depends: hspec-discover:hspec-discover >= 1.8\n  build-depends:\n    base       >= 2   && < 5,\n    containers >= 0.1 && < 0.7,\n    hspec      >= 2   && < 3,\n    QuickCheck >= 2   && < 3,\n    reflection\n";
    }