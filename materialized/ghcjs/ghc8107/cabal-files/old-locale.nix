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
      identifier = { name = "old-locale"; version = "1.0.0.7"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "libraries@haskell.org";
      author = "";
      homepage = "";
      url = "";
      synopsis = "locale library";
      description = "This package provides the ability to adapt to\nlocale conventions such as date and time formats.";
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
      url = "http://hackage.haskell.org/package/old-locale-1.0.0.7.tar.gz";
      sha256 = "dbaf8bf6b888fb98845705079296a23c3f40ee2f449df7312f7f7f1de18d7b50";
      });
    }) // {
    package-description-override = "name:         old-locale\r\nversion:      1.0.0.7\r\nx-revision: 2\r\n-- NOTE: Don't forget to update ./changelog.md\r\nlicense:      BSD3\r\nlicense-file: LICENSE\r\nmaintainer:   libraries@haskell.org\r\nbug-reports:  https://github.com/haskell/old-locale/issues\r\nsynopsis:     locale library\r\ncategory:     System\r\nbuild-type:   Simple\r\nCabal-Version:>=1.10\r\ntested-with:  GHC==7.8.3, GHC==7.8.2, GHC==7.8.1, GHC==7.6.3, GHC==7.6.2, GHC==7.6.1, GHC==7.4.2, GHC==7.4.1, GHC==7.2.2, GHC==7.2.1, GHC==7.0.4, GHC==7.0.3, GHC==7.0.2, GHC==7.0.1, GHC==6.12.3\r\ndescription:\r\n    This package provides the ability to adapt to\r\n    locale conventions such as date and time formats.\r\n\r\nextra-source-files:\r\n    changelog.md\r\n\r\nsource-repository head\r\n    type:     git\r\n    location: https://github.com/haskell/old-locale.git\r\n\r\nLibrary\r\n    default-language: Haskell98\r\n    other-extensions: CPP\r\n    if impl(ghc>=7.2)\r\n        -- && base>=4.4.1\r\n        other-extensions: Safe\r\n\r\n    exposed-modules:\r\n        System.Locale\r\n\r\n    build-depends: base >= 4.2 && < 5\r\n    ghc-options: -Wall\r\n";
    }