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
      identifier = { name = "cabal-doctest"; version = "1.0.8"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2017 Oleg Grenrus";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      homepage = "https://github.com/phadej/cabal-doctest";
      url = "";
      synopsis = "A Setup.hs helper for doctests running";
      description = "Currently (beginning of 2017), there isn't @cabal doctest@\ncommand. Yet, to properly work doctest needs plenty of configuration.\nThis library provides the common bits for writing custom Setup.hs\nSee <https://github.com/haskell/cabal/issues/2327 Cabal/2327> for the progress\nof @cabal doctest@, i.e. whether this library is obsolete.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/cabal-doctest-1.0.8.tar.gz";
      sha256 = "2026a6a87d410202ce091412ca6bc33c5aca787025326b4a3d13425a23392e0e";
      });
    }) // {
    package-description-override = "name:               cabal-doctest\nversion:            1.0.8\nx-revision:         2\nsynopsis:           A Setup.hs helper for doctests running\ndescription:\n  Currently (beginning of 2017), there isn't @cabal doctest@\n  command. Yet, to properly work doctest needs plenty of configuration.\n  This library provides the common bits for writing custom Setup.hs\n  See <https://github.com/haskell/cabal/issues/2327 Cabal/2327> for the progress\n  of @cabal doctest@, i.e. whether this library is obsolete.\n\nhomepage:           https://github.com/phadej/cabal-doctest\nlicense:            BSD3\nlicense-file:       LICENSE\nauthor:             Oleg Grenrus <oleg.grenrus@iki.fi>\nmaintainer:         Oleg Grenrus <oleg.grenrus@iki.fi>\ncopyright:          (c) 2017 Oleg Grenrus\ncategory:           Distribution\nbuild-type:         Simple\ncabal-version:      >=1.10\nextra-source-files:\n  ChangeLog.md\n  README.md\n\ntested-with:        GHC ==8.10.1 || >=7.4 && <8.10 || ==7.2.2 || ==7.0.4\n\nsource-repository head\n  type:     git\n  location: https://github.com/phadej/cabal-doctest\n\nlibrary\n  exposed-modules:  Distribution.Extra.Doctest\n  other-modules:\n  other-extensions:\n  build-depends:\n      base       >=4.3  && <4.16\n    , Cabal      >=1.10 && <3.6\n    , directory\n    , filepath\n\n  hs-source-dirs:   src\n  default-language: Haskell2010\n  ghc-options:      -Wall\n\n  if !impl(ghc >=7.2)\n    -- Work around a pattern-match coverage checking bug in GHC 7.0\n    ghc-options: -fno-warn-overlapping-patterns\n";
    }