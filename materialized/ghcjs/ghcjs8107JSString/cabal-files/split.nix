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
      identifier = { name = "split"; version = "0.2.3.4"; };
      license = "BSD-3-Clause";
      copyright = "(c) Brent Yorgey, Louis Wasserman 2008-2012";
      maintainer = "byorgey@gmail.com";
      author = "Brent Yorgey";
      homepage = "";
      url = "";
      synopsis = "Combinator library for splitting lists.";
      description = "A collection of various methods for splitting\nlists into parts, akin to the \\\"split\\\" function\nfound in several mainstream languages. Here is\nits tale:\n\nOnce upon a time the standard \"Data.List\" module\nheld no function for splitting a list into parts\naccording to a delimiter.  Many a brave\nlambda-knight strove to add such a function, but\ntheir striving was in vain, for Lo, the Supreme\nCouncil fell to bickering amongst themselves what\nwas to be the essential nature of the One True\nFunction which could cleave a list in twain (or\nthrain, or any required number of parts).\n\nAnd thus came to pass the split package,\ncomprising divers functions for splitting a list\nasunder, each according to its nature.  And the\nSupreme Council had no longer any grounds for\nargument, for the favored method of each was\ncontained therein.\n\nTo get started, see the \"Data.List.Split\" module.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      tests = {
        "split-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."split" or (errorHandler.buildDepError "split"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/split-0.2.3.4.tar.gz";
      sha256 = "271fe5104c9f40034aa9a1aad6269bcecc9454bc5a57c247e69e17de996c1f2a";
      });
    }) // {
    package-description-override = "Name:                split\r\nVersion:             0.2.3.4\r\nx-revision: 1\r\nStability:           stable\r\n\r\nDescription:         A collection of various methods for splitting\r\n                     lists into parts, akin to the \\\"split\\\" function\r\n                     found in several mainstream languages. Here is\r\n                     its tale:\r\n                     .\r\n                     Once upon a time the standard \"Data.List\" module\r\n                     held no function for splitting a list into parts\r\n                     according to a delimiter.  Many a brave\r\n                     lambda-knight strove to add such a function, but\r\n                     their striving was in vain, for Lo, the Supreme\r\n                     Council fell to bickering amongst themselves what\r\n                     was to be the essential nature of the One True\r\n                     Function which could cleave a list in twain (or\r\n                     thrain, or any required number of parts).\r\n                     .\r\n                     And thus came to pass the split package,\r\n                     comprising divers functions for splitting a list\r\n                     asunder, each according to its nature.  And the\r\n                     Supreme Council had no longer any grounds for\r\n                     argument, for the favored method of each was\r\n                     contained therein.\r\n                     .\r\n                     To get started, see the \"Data.List.Split\" module.\r\nSynopsis:            Combinator library for splitting lists.\r\nLicense:             BSD3\r\nLicense-file:        LICENSE\r\nCopyright:           (c) Brent Yorgey, Louis Wasserman 2008-2012\r\nExtra-source-files:  README, test/Properties.hs, CHANGES\r\nAuthor:              Brent Yorgey\r\nMaintainer:          byorgey@gmail.com\r\nCategory:            List\r\nBuild-type:          Simple\r\nCabal-Version:       >= 1.10\r\nTested-with:         GHC ==7.0.4 || ==7.2.2 || ==7.4.2 || ==7.6.3 || ==7.8.4 || ==7.10.3 || ==8.0.2 || ==8.2.2 || ==8.4.4 || ==8.6.5 || ==8.8.4 || ==8.10.4 || ==9.0.1\r\nBug-reports:         https://github.com/byorgey/split/issues\r\n\r\nTest-suite split-tests\r\n  type:              exitcode-stdio-1.0\r\n  main-is:           Properties.hs\r\n  build-depends:     base, QuickCheck >= 2.4, split\r\n  default-language:  Haskell2010\r\n  Hs-source-dirs:    test\r\n\r\nSource-repository head\r\n  type:              git\r\n  location:          http://github.com/byorgey/split.git\r\n\r\nLibrary\r\n  ghc-options:       -Wall\r\n  build-depends:     base < 4.16\r\n  exposed-modules:   Data.List.Split, Data.List.Split.Internals\r\n  default-language:  Haskell2010\r\n  Hs-source-dirs:    src\r\n";
    }