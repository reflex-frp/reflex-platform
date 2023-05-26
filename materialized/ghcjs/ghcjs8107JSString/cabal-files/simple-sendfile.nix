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
    flags = { allow-bsd = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "simple-sendfile"; version = "0.2.30"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Kazu Yamamoto <kazu@iij.ad.jp>";
      author = "Kazu Yamamoto <kazu@iij.ad.jp>";
      homepage = "";
      url = "";
      synopsis = "Cross platform library for the sendfile system call";
      description = "Cross platform library for the sendfile system call.\nThis library tries to call minimum system calls which\nare the bottleneck of web servers.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ] ++ (if system.isFreebsd && flags.allow-bsd
          then [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]
          else if system.isOsx
            then [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]
            else if system.isLinux
              then [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]
              else [
                (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
                (hsPkgs."conduit-extra" or (errorHandler.buildDepError "conduit-extra"))
                (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
                (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
                ]);
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
            (hsPkgs."conduit-extra" or (errorHandler.buildDepError "conduit-extra"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."simple-sendfile" or (errorHandler.buildDepError "simple-sendfile"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/simple-sendfile-0.2.30.tar.gz";
      sha256 = "b6864d2b3c62ff8ea23fa24e9e26f751bfe5253c8efb1f1e4fee2ba91d065284";
      });
    }) // {
    package-description-override = "Name:                   simple-sendfile\nVersion:                0.2.30\nAuthor:                 Kazu Yamamoto <kazu@iij.ad.jp>\nMaintainer:             Kazu Yamamoto <kazu@iij.ad.jp>\nLicense:                BSD3\nLicense-File:           LICENSE\nSynopsis:               Cross platform library for the sendfile system call\nDescription:            Cross platform library for the sendfile system call.\n                        This library tries to call minimum system calls which\n                        are the bottleneck of web servers.\nCategory:               Network\nCabal-Version:          >= 1.10\nBuild-Type:             Simple\n\nExtra-source-files:     test/inputFile\n\nFlag allow-bsd\n  Description:          Allow use of BSD sendfile (disable on GNU/kFreeBSD)\n  Default:              True\n\nLibrary\n  Default-Language:     Haskell2010\n  GHC-Options:          -Wall\n  Exposed-Modules:      Network.Sendfile\n  Other-Modules:        Network.Sendfile.Types\n  Build-Depends:        base >= 4.8 && < 5\n                      , network\n                      , bytestring\n  -- NetBSD and OpenBSD don't have sendfile\n  if os(freebsd) && flag(allow-bsd)\n    CPP-Options:        -DOS_BSD\n    Other-Modules:      Network.Sendfile.BSD\n                        Network.Sendfile.IOVec\n    Build-Depends:      unix\n  else\n    if os(darwin)\n      CPP-Options:      -DOS_MacOS\n      Other-Modules:    Network.Sendfile.BSD\n                        Network.Sendfile.IOVec\n      Build-Depends:    unix\n    else\n      if os(linux)\n        CPP-Options:    -DOS_Linux\n        Exposed-Modules: System.Linux.Sendfile\n        Other-Modules:  Network.Sendfile.Linux\n        Build-Depends:  unix\n      else\n        Other-Modules:  Network.Sendfile.Fallback\n        Build-Depends:  conduit         >= 1.0 && < 1.4\n                      , conduit-extra   >= 1.0 && < 1.4\n                      , transformers    >= 0.2.2 && < 0.6\n                      , resourcet\n\nTest-Suite spec\n  Type:                 exitcode-stdio-1.0\n  Default-Language:     Haskell2010\n  Hs-Source-Dirs:       test\n  Main-Is:              Spec.hs\n  GHC-Options:          -Wall\n  Other-Modules:        SendfileSpec\n  Build-Depends:        base\n                      , HUnit\n                      , bytestring\n                      , conduit\n                      , conduit-extra\n                      , resourcet\n                      , directory\n                      , hspec >= 1.3\n                      , network\n                      , process\n                      , simple-sendfile\n                      , unix\n\nSource-Repository head\n  Type:                 git\n  Location:             git://github.com/kazu-yamamoto/simple-sendfile\n";
    }