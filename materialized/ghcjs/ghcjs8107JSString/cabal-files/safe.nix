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
      specVersion = "1.18";
      identifier = { name = "safe"; version = "0.3.19"; };
      license = "BSD-3-Clause";
      copyright = "Neil Mitchell 2007-2020";
      maintainer = "Neil Mitchell <ndmitchell@gmail.com>";
      author = "Neil Mitchell <ndmitchell@gmail.com>";
      homepage = "https://github.com/ndmitchell/safe#readme";
      url = "";
      synopsis = "Library of safe (exception free) functions";
      description = "A library wrapping @Prelude@/@Data.List@ functions that can throw exceptions, such as @head@ and @!!@.\nEach unsafe function has up to four variants, e.g. with @tail@:\n\n* @tail :: [a] -> [a]@, raises an error on @tail []@.\n\n* @tailMay :: [a] -> /Maybe/ [a]@, turns errors into @Nothing@.\n\n* @tailDef :: /[a]/ -> [a] -> [a]@, takes a default to return on errors.\n\n* @tailNote :: /String/ -> [a] -> [a]@, takes an extra argument which supplements the error message.\n\n* @tailSafe :: [a] -> [a]@, returns some sensible default if possible, @[]@ in the case of @tail@.\n\nThis package is divided into three modules:\n\n* \"Safe\" contains safe variants of @Prelude@ and @Data.List@ functions.\n\n* \"Safe.Foldable\" contains safe variants of @Foldable@ functions.\n\n* \"Safe.Exact\" creates crashing versions of functions like @zip@ (errors if the lists are not equal) and @take@ (errors if there are not enough elements), then wraps them to provide safe variants.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      tests = {
        "safe-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/safe-0.3.19.tar.gz";
      sha256 = "25043442c8f8aa95955bb17467d023630632b961aaa61e807e325d9b2c33f7a2";
      });
    }) // {
    package-description-override = "cabal-version:  >= 1.18\nbuild-type:     Simple\nname:           safe\nversion:        0.3.19\nlicense:        BSD3\nlicense-file:   LICENSE\ncategory:       Unclassified\nauthor:         Neil Mitchell <ndmitchell@gmail.com>\nmaintainer:     Neil Mitchell <ndmitchell@gmail.com>\ncopyright:      Neil Mitchell 2007-2020\nhomepage:       https://github.com/ndmitchell/safe#readme\nsynopsis:       Library of safe (exception free) functions\nbug-reports:    https://github.com/ndmitchell/safe/issues\ntested-with:    GHC==8.10.1, GHC==8.8.3, GHC==8.6.5, GHC==8.4.4, GHC==8.2.2, GHC==8.0.2\ndescription:\n    A library wrapping @Prelude@/@Data.List@ functions that can throw exceptions, such as @head@ and @!!@.\n    Each unsafe function has up to four variants, e.g. with @tail@:\n    .\n    * @tail :: [a] -> [a]@, raises an error on @tail []@.\n    .\n    * @tailMay :: [a] -> /Maybe/ [a]@, turns errors into @Nothing@.\n    .\n    * @tailDef :: /[a]/ -> [a] -> [a]@, takes a default to return on errors.\n    .\n    * @tailNote :: /String/ -> [a] -> [a]@, takes an extra argument which supplements the error message.\n    .\n    * @tailSafe :: [a] -> [a]@, returns some sensible default if possible, @[]@ in the case of @tail@.\n    .\n    This package is divided into three modules:\n    .\n    * \"Safe\" contains safe variants of @Prelude@ and @Data.List@ functions.\n    .\n    * \"Safe.Foldable\" contains safe variants of @Foldable@ functions.\n    .\n    * \"Safe.Exact\" creates crashing versions of functions like @zip@ (errors if the lists are not equal) and @take@ (errors if there are not enough elements), then wraps them to provide safe variants.\nextra-doc-files:\n    CHANGES.txt\n    README.md\n\nsource-repository head\n    type:     git\n    location: https://github.com/ndmitchell/safe.git\n\nlibrary\n    default-language: Haskell2010\n    build-depends:\n        base >= 4.8 && < 5\n\n    exposed-modules:\n        Safe\n        Safe.Exact\n        Safe.Foldable\n        Safe.Partial\n\n    other-modules:\n        Safe.Util\n\ntest-suite safe-test\n    type:               exitcode-stdio-1.0\n    main-is:            Test.hs\n    default-language:   Haskell2010\n\n    other-modules:\n        Safe\n        Safe.Exact\n        Safe.Foldable\n        Safe.Partial\n        Safe.Util\n    build-depends:\n        base,\n        deepseq,\n        QuickCheck,\n        safe\n";
    }