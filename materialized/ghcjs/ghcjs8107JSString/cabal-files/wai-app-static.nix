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
    flags = { print = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "wai-app-static"; version = "3.1.7.2"; };
      license = "MIT";
      copyright = "";
      maintainer = "Michael Snoyman <michael@snoyman.com>, Greg Weber <greg@gregweber.info>";
      author = "Michael Snoyman <michael@snoyman.com>";
      homepage = "http://www.yesodweb.com/book/web-application-interface";
      url = "";
      synopsis = "WAI application for static serving";
      description = "API docs and the README are available at <http://www.stackage.org/package/wai-app-static>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
          (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."http-date" or (errorHandler.buildDepError "http-date"))
          (hsPkgs."blaze-html" or (errorHandler.buildDepError "blaze-html"))
          (hsPkgs."blaze-markup" or (errorHandler.buildDepError "blaze-markup"))
          (hsPkgs."mime-types" or (errorHandler.buildDepError "mime-types"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."wai-extra" or (errorHandler.buildDepError "wai-extra"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
          ];
        buildable = true;
        };
      exes = {
        "warp" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."wai-app-static" or (errorHandler.buildDepError "wai-app-static"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."mime-types" or (errorHandler.buildDepError "mime-types"))
            ];
          buildable = true;
          };
        };
      tests = {
        "runtests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
            (hsPkgs."http-date" or (errorHandler.buildDepError "http-date"))
            (hsPkgs."wai-app-static" or (errorHandler.buildDepError "wai-app-static"))
            (hsPkgs."wai-extra" or (errorHandler.buildDepError "wai-extra"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."mime-types" or (errorHandler.buildDepError "mime-types"))
            (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."mockery" or (errorHandler.buildDepError "mockery"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/wai-app-static-3.1.7.2.tar.gz";
      sha256 = "c8e7db8ddb31d2297df4cae0add63e514f2a8ef92a68541707585f8148690f8d";
      });
    }) // {
    package-description-override = "name:            wai-app-static\r\nversion:         3.1.7.2\r\nx-revision: 1\r\nlicense:         MIT\r\nlicense-file:    LICENSE\r\nauthor:          Michael Snoyman <michael@snoyman.com>\r\nmaintainer:      Michael Snoyman <michael@snoyman.com>, Greg Weber <greg@gregweber.info>\r\nsynopsis:        WAI application for static serving\r\ndescription:     API docs and the README are available at <http://www.stackage.org/package/wai-app-static>.\r\ncategory:        Web, Yesod\r\nstability:       Stable\r\ncabal-version:   >= 1.10\r\nbuild-type:      Simple\r\nhomepage:        http://www.yesodweb.com/book/web-application-interface\r\nExtra-source-files:\r\n  images/folder.png\r\n  images/haskell.png\r\n  test/*.hs\r\n  test/a/b\r\n  tests.hs\r\n  README.md\r\n  ChangeLog.md\r\n\r\nFlag print\r\n    Description:   print debug info\r\n    Default:       False\r\n\r\nlibrary\r\n    default-language: Haskell2010\r\n    build-depends:   base                      >= 4        && < 5\r\n                   , wai                       >= 3.0      && < 3.3\r\n                   , bytestring                >= 0.10.4\r\n                   , http-types                >= 0.7\r\n                   , transformers              >= 0.2.2\r\n                   , unix-compat               >= 0.2\r\n                   , directory                 >= 1.0.1\r\n                   , containers                >= 0.2\r\n                   , time                      >= 1.1.4\r\n                   , old-locale                >= 1.0.0.2\r\n                   , file-embed                >= 0.0.3.1\r\n                   , text                      >= 0.7\r\n                   , cryptonite                >= 0.6\r\n                   , memory                    >= 0.7\r\n                   , http-date\r\n                   , blaze-html                >= 0.5\r\n                   , blaze-markup              >= 0.5.1\r\n                   , mime-types                >= 0.1      && < 0.2\r\n                   , unordered-containers      >= 0.2\r\n                   , template-haskell          >= 2.7\r\n                   , zlib                      >= 0.5\r\n                   , filepath\r\n                   , wai-extra                 >= 3.0      && < 3.2\r\n                   , optparse-applicative      >= 0.7\r\n                   , warp                      >= 3.0.11   && < 3.4\r\n\r\n    exposed-modules: Network.Wai.Application.Static\r\n                     WaiAppStatic.Storage.Filesystem\r\n                     WaiAppStatic.Storage.Embedded\r\n                     WaiAppStatic.Listing\r\n                     WaiAppStatic.Types\r\n                     WaiAppStatic.CmdLine\r\n    other-modules:   Util\r\n                     WaiAppStatic.Storage.Embedded.Runtime\r\n                     WaiAppStatic.Storage.Embedded.TH\r\n    ghc-options:     -Wall\r\n\r\n    if flag(print)\r\n      cpp-options:  -DPRINT\r\n\r\nExecutable warp\r\n  default-language: Haskell2010\r\n  Main-is:        warp-static.hs\r\n  hs-source-dirs: app\r\n  Build-depends: base            >= 4                  && < 5\r\n               , wai-app-static\r\n               , directory       >= 1.0\r\n               , containers      >= 0.2\r\n               , bytestring      >= 0.10.4\r\n               , text            >= 0.7\r\n               , mime-types      >= 0.1                && < 0.2\r\n\r\ntest-suite runtests\r\n    default-language: Haskell2010\r\n    hs-source-dirs: test\r\n    main-is: ../tests.hs\r\n    type: exitcode-stdio-1.0\r\n\r\n    build-depends:   base                      >= 4        && < 5\r\n                   , hspec                     >= 1.3\r\n                   , unix-compat\r\n                   , time\r\n                   , old-locale\r\n                   , http-date\r\n                   , wai-app-static\r\n                   , wai-extra\r\n                   , wai\r\n                   , http-types\r\n                   , network\r\n                   , bytestring\r\n                   , text\r\n                   , transformers\r\n                   , mime-types\r\n                   , zlib\r\n                   , filepath\r\n                   , temporary\r\n                   , mockery\r\n                   -- , containers\r\n    ghc-options:   -Wall\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: git://github.com/yesodweb/wai.git\r\n";
    }