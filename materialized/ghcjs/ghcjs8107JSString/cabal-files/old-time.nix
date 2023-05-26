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
      identifier = { name = "old-time"; version = "1.1.0.3"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "libraries@haskell.org";
      author = "";
      homepage = "";
      url = "";
      synopsis = "Time library";
      description = "This package provides the old time library.\n\nFor new projects, the newer\n<http://hackage.haskell.org/package/time time library>\nis recommended.";
      buildType = "Configure";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/old-time-1.1.0.3.tar.gz";
      sha256 = "1ccb158b0f7851715d36b757c523b026ca1541e2030d02239802ba39b4112bc1";
      });
    }) // {
    package-description-override = "name:           old-time\r\nversion:        1.1.0.3\r\nx-revision: 2\r\n-- NOTE: Don't forget to update ./changelog.md\r\nlicense:        BSD3\r\nlicense-file:   LICENSE\r\nmaintainer:     libraries@haskell.org\r\nbug-reports:    https://github.com/haskell/old-time/issues\r\nsynopsis:       Time library\r\ncategory:       System\r\nbuild-type:     Configure\r\ncabal-Version:  >=1.10\r\ndescription:\r\n    This package provides the old time library.\r\n    .\r\n    For new projects, the newer\r\n    <http://hackage.haskell.org/package/time time library>\r\n    is recommended.\r\n\r\nextra-source-files:\r\n    aclocal.m4\r\n    changelog.md\r\n    config.guess\r\n    config.sub\r\n    configure\r\n    configure.ac\r\n    include/HsTimeConfig.h.in\r\n    install-sh\r\n    old-time.buildinfo\r\n\r\nextra-tmp-files:\r\n    autom4te.cache\r\n    config.log\r\n    config.status\r\n    include/HsTimeConfig.h\r\n\r\nsource-repository head\r\n    type:     git\r\n    location: https://github.com/haskell/old-time.git\r\n\r\nLibrary\r\n    default-language: Haskell2010\r\n    other-extensions: Trustworthy\r\n\r\n    exposed-modules:\r\n        System.Time\r\n\r\n    c-sources:\r\n        cbits/timeUtils.c\r\n\r\n    include-dirs: include\r\n    includes:     HsTime.h\r\n    install-includes:\r\n        HsTime.h\r\n\r\n    build-depends:\r\n        base       >= 4.7 && < 5,\r\n        old-locale == 1.0.*\r\n\r\n    ghc-options: -Wall\r\n";
    }