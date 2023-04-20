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
      identifier = { name = "optparse-applicative"; version = "0.15.1.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2012-2017 Paolo Capriotti <paolo@capriotti.io>";
      maintainer = "huw.campbell@gmail.com";
      author = "Paolo Capriotti, Huw Campbell";
      homepage = "https://github.com/pcapriotti/optparse-applicative";
      url = "";
      synopsis = "Utilities and combinators for parsing command line options";
      description = "optparse-applicative is a haskell library for parsing options\non the command line, providing a powerful applicative interface\nfor composing these options.\n\noptparse-applicative takes care of reading and validating the\narguments passed to the command line, handling and reporting\nerrors, generating a usage line, a comprehensive help screen,\nand enabling context-sensitive bash completions.\n\nSee the included README for detailed instructions and examples,\nwhich is also available on github\n<https://github.com/pcapriotti/optparse-applicative>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
          ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8")) [
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/optparse-applicative-0.15.1.0.tar.gz";
      sha256 = "4db3675fd1e0594afdf079db46f4cd412d483835d703e7c07e1a1a37d6f046f3";
      });
    }) // {
    package-description-override = "name:                optparse-applicative\r\nversion:             0.15.1.0\r\nx-revision: 1\r\nsynopsis:            Utilities and combinators for parsing command line options\r\ndescription:\r\n    optparse-applicative is a haskell library for parsing options\r\n    on the command line, providing a powerful applicative interface\r\n    for composing these options.\r\n    .\r\n    optparse-applicative takes care of reading and validating the\r\n    arguments passed to the command line, handling and reporting\r\n    errors, generating a usage line, a comprehensive help screen,\r\n    and enabling context-sensitive bash completions.\r\n    .\r\n    See the included README for detailed instructions and examples,\r\n    which is also available on github\r\n    <https://github.com/pcapriotti/optparse-applicative>.\r\nlicense:             BSD3\r\nlicense-file:        LICENSE\r\nauthor:              Paolo Capriotti, Huw Campbell\r\nmaintainer:          huw.campbell@gmail.com\r\ncopyright:           (c) 2012-2017 Paolo Capriotti <paolo@capriotti.io>\r\ncategory:            System, CLI, Options, Parsing\r\nbuild-type:          Simple\r\ncabal-version:       >= 1.8\r\nextra-source-files:  CHANGELOG.md\r\n                     README.md\r\n                     tests/alt.err.txt\r\n                     tests/cabal.err.txt\r\n                     tests/carry.err.txt\r\n                     tests/commands.err.txt\r\n                     tests/commands_header.err.txt\r\n                     tests/commands_header_full.err.txt\r\n                     tests/dropback.err.txt\r\n                     tests/hello.err.txt\r\n                     tests/helponempty.err.txt\r\n                     tests/helponemptysub.err.txt\r\n                     tests/long_equals.err.txt\r\n                     tests/formatting.err.txt\r\n                     tests/nested.err.txt\r\n                     tests/optional.err.txt\r\n                     tests/nested_optional.err.txt\r\n                     tests/subparsers.err.txt\r\n\r\nhomepage:            https://github.com/pcapriotti/optparse-applicative\r\nbug-reports:         https://github.com/pcapriotti/optparse-applicative/issues\r\ntested-with:\r\n  GHC==7.0.4,\r\n  GHC==7.2.2,\r\n  GHC==7.4.2,\r\n  GHC==7.6.3,\r\n  GHC==7.8.4,\r\n  GHC==7.10.3,\r\n  GHC==8.0.2,\r\n  GHC==8.2.2,\r\n  GHC==8.4.4,\r\n  GHC==8.6.5,\r\n  GHC==8.8.1\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/pcapriotti/optparse-applicative.git\r\n\r\nlibrary\r\n  hs-source-dirs:      src\r\n  ghc-options:         -Wall\r\n\r\n  -- See https://ghc.haskell.org/trac/ghc/wiki/Migration/8.0#base-4.9.0.0\r\n  if impl(ghc >= 8.0)\r\n    ghc-options:  -Wno-redundant-constraints -Wcompat -Wnoncanonical-monad-instances\r\n    if impl(ghc < 8.8)\r\n      ghc-options: -Wnoncanonical-monadfail-instances\r\n\r\n  exposed-modules:     Options.Applicative\r\n                     , Options.Applicative.Arrows\r\n                     , Options.Applicative.BashCompletion\r\n                     , Options.Applicative.Builder\r\n                     , Options.Applicative.Builder.Completer\r\n                     , Options.Applicative.Builder.Internal\r\n                     , Options.Applicative.Common\r\n                     , Options.Applicative.Extra\r\n                     , Options.Applicative.Help\r\n                     , Options.Applicative.Help.Chunk\r\n                     , Options.Applicative.Help.Core\r\n                     , Options.Applicative.Help.Levenshtein\r\n                     , Options.Applicative.Help.Pretty\r\n                     , Options.Applicative.Help.Types\r\n                     , Options.Applicative.Types\r\n                     , Options.Applicative.Internal\r\n\r\n  build-depends:       base                            == 4.*\r\n                     , transformers                    >= 0.2 && < 0.6\r\n                     , transformers-compat             >= 0.3 && < 0.7\r\n                     , process                         >= 1.0 && < 1.7\r\n                     , ansi-wl-pprint                  >= 0.6.8 && < 0.7\r\n\r\n  if !impl(ghc >= 8)\r\n    build-depends:     semigroups                      >= 0.10 && < 0.20\r\n                     , fail                            == 4.9.*\r\n\r\ntest-suite tests\r\n  type:                exitcode-stdio-1.0\r\n\r\n  main-is:             test.hs\r\n\r\n  ghc-options:         -Wall -threaded -O2 -funbox-strict-fields\r\n\r\n  hs-source-dirs:\r\n                       tests\r\n\r\n  other-modules:       Examples.Alternatives\r\n                     , Examples.Cabal\r\n                     , Examples.Commands\r\n                     , Examples.Formatting\r\n                     , Examples.Hello\r\n\r\n  build-depends:       base\r\n                     , bytestring                      >= 0.9 && < 0.11\r\n                     , optparse-applicative\r\n                     , QuickCheck                      >= 2.8 && < 2.15\r\n\r\n  if !impl(ghc >= 8)\r\n    build-depends:     semigroups\r\n";
    }