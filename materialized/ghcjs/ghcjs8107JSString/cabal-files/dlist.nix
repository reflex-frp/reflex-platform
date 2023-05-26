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
      identifier = { name = "dlist"; version = "0.8.0.8"; };
      license = "BSD-3-Clause";
      copyright = "2006-2009 Don Stewart, 2013-2019 Sean Leather";
      maintainer = "Sean Leather <sean.leather@gmail.com>";
      author = "Don Stewart";
      homepage = "https://github.com/spl/dlist";
      url = "";
      synopsis = "Difference lists";
      description = "Difference lists are a list-like type supporting O(1) append. This is\nparticularly useful for efficient logging and pretty printing (e.g. with the\nWriter monad), where list append quickly becomes too expensive.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/dlist-0.8.0.8.tar.gz";
      sha256 = "7129cf18068d3384e305708a10426ab8f573bee1030b023a114f45f1d0ec496d";
      });
    }) // {
    package-description-override = "name:                   dlist\nversion:                0.8.0.8\nsynopsis:               Difference lists\ndescription:\n  Difference lists are a list-like type supporting O(1) append. This is\n  particularly useful for efficient logging and pretty printing (e.g. with the\n  Writer monad), where list append quickly becomes too expensive.\ncategory:               Data\nlicense:                BSD3\nlicense-file:           LICENSE\nauthor:                 Don Stewart\nmaintainer:             Sean Leather <sean.leather@gmail.com>\ncopyright:              2006-2009 Don Stewart, 2013-2019 Sean Leather\nhomepage:               https://github.com/spl/dlist\nbug-reports:            https://github.com/spl/dlist/issues\nextra-source-files:     README.md,\n                        ChangeLog.md\nbuild-type:             Simple\ncabal-version:          >= 1.9.2\ntested-with:            GHC==7.0.4\n                        GHC==7.2.2\n                        GHC==7.4.2\n                        GHC==7.6.3\n                        GHC==7.8.4\n                        GHC==7.10.3\n                        GHC==8.0.2\n                        GHC==8.2.2\n                        GHC==8.4.4\n                        GHC==8.6.5\n                        GHC==8.8.1\n\nsource-repository head\n  type:                 git\n  location:             git://github.com/spl/dlist.git\n\nlibrary\n  build-depends:\n                        base >= 4 && < 5,\n                        deepseq >= 1.1 && < 1.5\n  extensions:           CPP\n  exposed-modules:      Data.DList\n  ghc-options:          -Wall\n\ntest-suite test\n  type:                 exitcode-stdio-1.0\n  main-is:              Main.hs\n  other-modules:        OverloadedStrings\n  hs-source-dirs:       tests\n  build-depends:        dlist,\n                        base,\n                        Cabal,\n                        -- QuickCheck-2.10 is the first version supporting\n                        -- base-4.9 (ghc-8) without the Arbitrary NonEmpty\n                        -- instance, which we include ourselves.\n                        QuickCheck >= 2.10 && < 2.15\n";
    }