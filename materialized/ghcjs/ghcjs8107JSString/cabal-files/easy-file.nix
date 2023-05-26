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
      specVersion = "1.6";
      identifier = { name = "easy-file"; version = "0.2.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Kazu Yamamoto <kazu@iij.ad.jp>";
      author = "Kazu Yamamoto <kazu@iij.ad.jp>";
      homepage = "http://github.com/kazu-yamamoto/easy-file";
      url = "";
      synopsis = "Cross-platform File handling";
      description = "Cross-platform File handling for Unix\\/Mac\\/Windows";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (if system.isWindows
          then [
            (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            ]
          else [
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            ]);
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/easy-file-0.2.2.tar.gz";
      sha256 = "52f52e72ba48d60935932401c233a72bf45c582871238aecc5a18021ce67b47e";
      });
    }) // {
    package-description-override = "Name:                   easy-file\nVersion:                0.2.2\nAuthor:                 Kazu Yamamoto <kazu@iij.ad.jp>\nMaintainer:             Kazu Yamamoto <kazu@iij.ad.jp>\nLicense:                BSD3\nLicense-File:           LICENSE\nSynopsis:               Cross-platform File handling\nDescription:            Cross-platform File handling for Unix\\/Mac\\/Windows\nHomepage:               http://github.com/kazu-yamamoto/easy-file\nCategory:               System\nCabal-Version:          >= 1.6\nBuild-Type:             Simple\n\nLibrary\n  GHC-Options:          -Wall\n  Exposed-Modules:      System.EasyFile\n  Other-Modules:        System.EasyFile.FilePath\n                        System.EasyFile.Directory\n                        System.EasyFile.Missing\n  Build-Depends:        base >= 4 && < 5\n  if os(windows)\n    Build-Depends:      Win32, time, directory, filepath\n  else\n    Build-Depends:      unix, time, directory, filepath\n\nSource-Repository head\n  Type:                 git\n  Location:             git://github.com/kazu-yamamoto/easy-file.git\n";
    }