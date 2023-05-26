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
      identifier = { name = "fast-logger"; version = "3.0.3"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Kazu Yamamoto <kazu@iij.ad.jp>";
      author = "Kazu Yamamoto <kazu@iij.ad.jp>";
      homepage = "https://github.com/kazu-yamamoto/logger";
      url = "";
      synopsis = "A fast logging system";
      description = "A fast logging system for Haskell";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."auto-update" or (errorHandler.buildDepError "auto-update"))
          (hsPkgs."easy-file" or (errorHandler.buildDepError "easy-file"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."unix-time" or (errorHandler.buildDepError "unix-time"))
          (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.8") (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"));
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."fast-logger" or (errorHandler.buildDepError "fast-logger"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/fast-logger-3.0.3.tar.gz";
      sha256 = "5763a0321053ecaba2d1040800bae9988f52b813fb08d5276ea7ce10e3d2f068";
      });
    }) // {
    package-description-override = "Name:                   fast-logger\nVersion:                3.0.3\nAuthor:                 Kazu Yamamoto <kazu@iij.ad.jp>\nMaintainer:             Kazu Yamamoto <kazu@iij.ad.jp>\nLicense:                BSD3\nLicense-File:           LICENSE\nSynopsis:               A fast logging system\nDescription:            A fast logging system for Haskell\nHomepage:               https://github.com/kazu-yamamoto/logger\nCategory:               System\nCabal-Version:          >= 1.10\nBuild-Type:             Simple\nExtra-Source-Files:     README.md ChangeLog.md\nTested-With:            GHC ==7.8.4 || ==7.10.3 || ==8.0.2 || ==8.2.2 || ==8.4.4 || ==8.6.3\n\nLibrary\n  Default-Language:     Haskell2010\n  GHC-Options:          -Wall\n  Exposed-Modules:      System.Log.FastLogger\n                        System.Log.FastLogger.Date\n                        System.Log.FastLogger.File\n                        System.Log.FastLogger.Internal\n                        System.Log.FastLogger.LoggerSet\n                        System.Log.FastLogger.Types\n  Other-Modules:        System.Log.FastLogger.Imports\n                        System.Log.FastLogger.IO\n                        System.Log.FastLogger.FileIO\n                        System.Log.FastLogger.LogStr\n                        System.Log.FastLogger.Logger\n  Build-Depends:        base >= 4.9 && < 5\n                      , array\n                      , auto-update >= 0.1.2\n                      , easy-file >= 0.2\n                      , bytestring >= 0.10.4\n                      , directory\n                      , filepath\n                      , text\n                      , unix-time >= 0.4.4\n                      , unix-compat\n  if impl(ghc < 7.8)\n      Build-Depends:    bytestring-builder\n  if impl(ghc >= 8)\n      Default-Extensions:  Strict StrictData\n\nTest-Suite spec\n  Main-Is:          Spec.hs\n  Hs-Source-Dirs:   test\n  Default-Language: Haskell2010\n  Type:             exitcode-stdio-1.0\n\n  Ghc-Options:      -Wall -threaded\n  Other-Modules:    FastLoggerSpec\n  Build-Tools:      hspec-discover >= 2.6\n  Build-Depends:    base >= 4 && < 5\n                  , bytestring >= 0.10.4\n                  , directory\n                  , fast-logger\n                  , hspec\n  if impl(ghc >= 8)\n      Default-Extensions:  Strict StrictData\n\nSource-Repository head\n  Type:                 git\n  Location:             git://github.com/kazu-yamamoto/logger.git\n";
    }