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
      identifier = { name = "mime-types"; version = "0.1.0.9"; };
      license = "MIT";
      copyright = "";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman";
      homepage = "https://github.com/yesodweb/wai";
      url = "";
      synopsis = "Basic mime-type handling types and functions";
      description = "API docs and the README are available at <http://www.stackage.org/package/mime-types>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/mime-types-0.1.0.9.tar.gz";
      sha256 = "0a32435169ef4ba59f4a4b8addfd0c04479410854d1b8d69a1e38fb389ba71d2";
      });
    }) // {
    package-description-override = "name:                mime-types\nversion:             0.1.0.9\nsynopsis:            Basic mime-type handling types and functions\ndescription:         API docs and the README are available at <http://www.stackage.org/package/mime-types>.\nhomepage:            https://github.com/yesodweb/wai\nlicense:             MIT\nlicense-file:        LICENSE\nauthor:              Michael Snoyman\nmaintainer:          michael@snoyman.com\ncategory:            Web\nbuild-type:          Simple\ncabal-version:       >=1.8\nextra-source-files:  README.md ChangeLog.md\n\nlibrary\n  exposed-modules:     Network.Mime\n  build-depends:       base           >= 4      && < 5\n                     , containers\n                     , text\n                     , bytestring\n\nsource-repository head\n  type:     git\n  location: git://github.com/yesodweb/wai.git\n";
    }