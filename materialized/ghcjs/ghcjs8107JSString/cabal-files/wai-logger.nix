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
      identifier = { name = "wai-logger"; version = "2.3.6"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Kazu Yamamoto <kazu@iij.ad.jp>";
      author = "Kazu Yamamoto <kazu@iij.ad.jp>";
      homepage = "";
      url = "";
      synopsis = "A logging system for WAI";
      description = "A logging system for WAI";
      buildType = "Custom";
      setup-depends = [
        (hsPkgs.buildPackages.base or (pkgs.buildPackages.base or (errorHandler.setupDepError "base")))
        (hsPkgs.buildPackages.Cabal or (pkgs.buildPackages.Cabal or (errorHandler.setupDepError "Cabal")))
        (hsPkgs.buildPackages.cabal-doctest or (pkgs.buildPackages.cabal-doctest or (errorHandler.setupDepError "cabal-doctest")))
        ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."byteorder" or (errorHandler.buildDepError "byteorder"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."fast-logger" or (errorHandler.buildDepError "fast-logger"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
          ];
        buildable = true;
        };
      tests = {
        "doctests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."wai-logger" or (errorHandler.buildDepError "wai-logger"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/wai-logger-2.3.6.tar.gz";
      sha256 = "e2fbd8c74fa0a31f9ea0faa53f4ad4e588644a34d8dfc7cc50d85c245c3c7541";
      });
    }) // {
    package-description-override = "Name:                   wai-logger\nVersion:                2.3.6\nAuthor:                 Kazu Yamamoto <kazu@iij.ad.jp>\nMaintainer:             Kazu Yamamoto <kazu@iij.ad.jp>\nLicense:                BSD3\nLicense-File:           LICENSE\nSynopsis:               A logging system for WAI\nDescription:            A logging system for WAI\nCategory:               Web, Yesod\nCabal-Version:          >= 1.10\nBuild-Type:             Custom\nTested-With:            GHC ==7.8.4 || ==7.10.3 || ==8.0.2 || ==8.2.2 || ==8.4.4 || ==8.6.3\n\nCustom-Setup\n  Setup-Depends:        base, Cabal, cabal-doctest >=1.0.6 && <1.1\n\nLibrary\n  Default-Language:     Haskell2010\n  GHC-Options:          -Wall\n  Exposed-Modules:      Network.Wai.Logger\n  Other-Modules:        Network.Wai.Logger.Apache\n                        Network.Wai.Logger.IP\n                        Network.Wai.Logger.IORef\n  Build-Depends:        base >= 4 && < 5\n                      , byteorder\n                      , bytestring\n                      , fast-logger >= 3\n                      , http-types\n                      , network\n                      , wai >= 2.0.0\n if impl(ghc >= 8)\n      Default-Extensions:  Strict StrictData\n\nTest-Suite doctests\n  Type:                 exitcode-stdio-1.0\n  Default-Language:     Haskell2010\n  HS-Source-Dirs:       test\n  Ghc-Options:          -Wall\n  Main-Is:              doctests.hs\n  Build-Depends:        base\n                      , wai-logger\n                      , doctest >= 0.10.1\n  if impl(ghc >= 8)\n      Default-Extensions:  Strict StrictData\n\nSource-Repository head\n  Type:                 git\n  Location:             git://github.com/kazu-yamamoto/logger.git\n";
    }