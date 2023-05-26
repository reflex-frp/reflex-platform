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
      specVersion = "1.2";
      identifier = { name = "extensible-exceptions"; version = "0.1.1.4"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "libraries@haskell.org";
      author = "";
      homepage = "";
      url = "";
      synopsis = "Extensible exceptions";
      description = "This package provides extensible exceptions for both new and\nold versions of GHC (i.e., < 6.10).";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/extensible-exceptions-0.1.1.4.tar.gz";
      sha256 = "6ce5e8801760385a408dab71b53550f87629e661b260bdc2cd41c6a439b6e388";
      });
    }) // {
    package-description-override = "name:           extensible-exceptions\nversion:        0.1.1.4\nlicense:        BSD3\nlicense-file:   LICENSE\nmaintainer:     libraries@haskell.org\nbug-reports: http://hackage.haskell.org/trac/ghc/newticket?component=libraries%20%28other%29\nsynopsis:       Extensible exceptions\ncategory:       Control\ndescription:\n    This package provides extensible exceptions for both new and \n    old versions of GHC (i.e., < 6.10).\n-- We want to remain compatible with older Cabal versions for now,\n-- but we need at least 1.2.3 in order to list DeriveDataTypeable\n-- as an extension.\n-- (GHC trac #3931)\n-- cabal-version:  >=1.6\ncabal-version:  >=1.2.3\nbuild-type: Simple\n\n-- We want to remain compatible with older Cabal versions for now,\n-- and they don't understand source-repository sections\n-- (GHC trac #3931)\n-- source-repository head\n--     type:     git\n--     location: http://darcs.haskell.org/packages/extensible-exceptions.git/\n\nLibrary {\n    if impl(ghc>=6.9)\n        cpp-options: -DUSE_NEW_EXCEPTIONS\n        build-depends: base>=4&&<5\n    else\n        build-depends: base<4\n    exposed-modules:\n        Control.Exception.Extensible\n    extensions: CPP, ExistentialQuantification, DeriveDataTypeable\n}\n";
    }