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
      identifier = { name = "unix-time"; version = "0.4.7"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Kazu Yamamoto <kazu@iij.ad.jp>";
      author = "Kazu Yamamoto <kazu@iij.ad.jp>";
      homepage = "";
      url = "";
      synopsis = "Unix time parser/formatter and utilities";
      description = "Fast parser\\/formatter\\/utilities for Unix time";
      buildType = "Configure";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."old-time" or (errorHandler.buildDepError "old-time"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          ];
        build-tools = [
          (hsPkgs.buildPackages.hsc2hs.components.exes.hsc2hs or (pkgs.buildPackages.hsc2hs or (errorHandler.buildToolDepError "hsc2hs:hsc2hs")))
          ];
        buildable = true;
        };
      tests = {
        "doctests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."unix-time" or (errorHandler.buildDepError "unix-time"))
            ];
          buildable = false;
          };
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
            (hsPkgs."old-time" or (errorHandler.buildDepError "old-time"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."unix-time" or (errorHandler.buildDepError "unix-time"))
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
      url = "http://hackage.haskell.org/package/unix-time-0.4.7.tar.gz";
      sha256 = "19233f8badf921d444c6165689253d877cfed58ce08f28cad312558a9280de09";
      });
    }) // {
    package-description-override = "Name:                   unix-time\nVersion:                0.4.7\nAuthor:                 Kazu Yamamoto <kazu@iij.ad.jp>\nMaintainer:             Kazu Yamamoto <kazu@iij.ad.jp>\nLicense:                BSD3\nLicense-File:           LICENSE\nSynopsis:               Unix time parser/formatter and utilities\nDescription:            Fast parser\\/formatter\\/utilities for Unix time\nCategory:               Data\nCabal-Version:          1.18\nBuild-Type:             Configure\nExtra-Source-Files:     cbits/config.h.in\n                        cbits/conv.c\n                        cbits/strftime.c\n                        cbits/strptime.c\n                        cbits/win_patch.c\n                        cbits/win_patch.h\n                        configure\n                        configure.ac\nExtra-Tmp-Files:        config.log config.status autom4te.cache cbits/config.h\n\nLibrary\n  Default-Language:     Haskell2010\n  GHC-Options:          -Wall\n  if impl(ghc >= 7.8)\n    CC-Options:         -fPIC\n  Exposed-Modules:      Data.UnixTime\n  Other-Modules:        Data.UnixTime.Conv\n                        Data.UnixTime.Diff\n                        Data.UnixTime.Types\n                        Data.UnixTime.Sys\n  Build-Depends:        base >= 4 && < 5\n                      , bytestring\n                      , old-time\n                      , binary\n  Build-Tools:          hsc2hs\n  C-Sources:            cbits/conv.c\n  if os(windows)\n    C-Sources:          cbits/strftime.c\n                      , cbits/strptime.c\n                      , cbits/win_patch.c\n  include-dirs:         cbits\n\nTest-Suite doctests\n  Buildable:            False\n  Type:                 exitcode-stdio-1.0\n  Default-Language:     Haskell2010\n  HS-Source-Dirs:       test\n  Ghc-Options:          -threaded -Wall\n  Main-Is:              doctests.hs\n  Build-Depends:        base\n                      , doctest >= 0.9.3\n                      , unix-time\n\nTest-Suite spec\n  Type:                 exitcode-stdio-1.0\n  Default-Language:     Haskell2010\n  Hs-Source-Dirs:       test\n  Ghc-Options:          -Wall\n  Main-Is:              Spec.hs\n  Other-Modules:        UnixTimeSpec\n  Build-Tools:          hspec-discover >= 2.6\n  Build-Depends:        base\n                      , bytestring\n                      , old-locale\n                      , old-time\n                      , QuickCheck\n                      , time\n                      , unix-time\n                      , hspec >= 2.6\n\nSource-Repository head\n  Type:                 git\n  Location:             https://github.com/kazu-yamamoto/unix-time\n";
    }