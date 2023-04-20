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
      identifier = { name = "http-date"; version = "0.0.11"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Kazu Yamamoto <kazu@iij.ad.jp>";
      author = "Kazu Yamamoto <kazu@iij.ad.jp>";
      homepage = "";
      url = "";
      synopsis = "HTTP Date parser/formatter";
      description = "Fast parser and formatter for HTTP Date";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ];
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."http-date" or (errorHandler.buildDepError "http-date"))
            (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          };
        "doctests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/http-date-0.0.11.tar.gz";
      sha256 = "32f923ac1ad9bdfeadce7c52a03c9ba6225ba60dc14137cb1cdf32ea84ccf4d3";
      });
    }) // {
    package-description-override = "Name:                   http-date\nVersion:                0.0.11\nAuthor:                 Kazu Yamamoto <kazu@iij.ad.jp>\nMaintainer:             Kazu Yamamoto <kazu@iij.ad.jp>\nLicense:                BSD3\nLicense-File:           LICENSE\nSynopsis:               HTTP Date parser/formatter\nDescription:            Fast parser and formatter for HTTP Date\nCategory:               Network, Web\nCabal-Version:          >= 1.10\nBuild-Type:             Simple\n\nLibrary\n  Default-Language:     Haskell2010\n  GHC-Options:          -Wall\n  Exposed-Modules:      Network.HTTP.Date\n  Other-Modules:        Network.HTTP.Date.Converter\n                        Network.HTTP.Date.Formatter\n                        Network.HTTP.Date.Types\n                        Network.HTTP.Date.Parser\n  Build-Depends:        base >= 4.9 && < 5\n                      , array\n                      , attoparsec\n                      , bytestring\n                      , time\n\nTest-Suite spec\n  Default-Language:     Haskell2010\n  Type:                 exitcode-stdio-1.0\n  HS-Source-Dirs:       test\n  Main-Is:              Spec.hs\n  Other-Modules:        DateSpec\n                        Model\n  Build-Depends:        base >= 4.9 && < 5\n                      , bytestring\n                      , hspec\n                      , http-date\n                      , old-locale\n                      , time\n\nTest-Suite doctests\n  Default-Language:     Haskell2010\n  Type:                 exitcode-stdio-1.0\n  HS-Source-Dirs:       test\n  Ghc-Options:          -threaded\n  Main-Is:              doctests.hs\n  Build-Depends:        base >= 4.9\n                      , doctest >= 0.8\n\nSource-Repository head\n  Type:                 git\n  Location:             git://github.com/kazu-yamamoto/http-date\n";
    }