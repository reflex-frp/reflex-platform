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
    flags = { example = true; };
    package = {
      specVersion = "1.8";
      identifier = { name = "wai-websockets"; version = "3.0.1.2"; };
      license = "MIT";
      copyright = "";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman, Jasper Van der Jeugt, Ting-Yen Lai";
      homepage = "http://github.com/yesodweb/wai";
      url = "";
      synopsis = "Provide a bridge between WAI and the websockets package.";
      description = "API docs and the README are available at <http://www.stackage.org/package/wai-websockets>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
          (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          ];
        buildable = true;
        };
      exes = {
        "wai-websockets-example" = {
          depends = (pkgs.lib).optionals (flags.example) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."wai-websockets" or (errorHandler.buildDepError "wai-websockets"))
            (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."wai-app-static" or (errorHandler.buildDepError "wai-app-static"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            ];
          buildable = if flags.example then true else false;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/wai-websockets-3.0.1.2.tar.gz";
      sha256 = "917cceb08f296d7dc6b6cafb66133ae53888b2c98b8fb2a2d7fa629d75ab5d2c";
      });
    }) // {
    package-description-override = "Name:                wai-websockets\nVersion:             3.0.1.2\nSynopsis:            Provide a bridge between WAI and the websockets package.\nLicense:             MIT\nLicense-file:        LICENSE\nAuthor:              Michael Snoyman, Jasper Van der Jeugt, Ting-Yen Lai\nMaintainer:          michael@snoyman.com\nHomepage:            http://github.com/yesodweb/wai\nCategory:            Web, Yesod\nBuild-Type:          Simple\nCabal-Version:       >=1.8\nStability:           Stable\ndescription:         API docs and the README are available at <http://www.stackage.org/package/wai-websockets>.\n\nextra-source-files: static/client.js, static/client.html, static/screen.css\n                    README.md ChangeLog.md\n\nflag example\n\nLibrary\n  Build-Depends:     base               >= 3        && < 5\n                   , bytestring         >= 0.9.1.4\n                   , wai                >= 3.0      && < 3.3\n                   , case-insensitive   >= 0.2\n                   , network            >= 2.2.1.5\n                   , transformers       >= 0.2\n                   , websockets         >= 0.9\n                   , http-types\n  Exposed-modules:   Network.Wai.Handler.WebSockets\n  ghc-options:       -Wall\n\nExecutable           wai-websockets-example\n  if flag(example)\n    buildable: True\n    Build-Depends:   base               >= 3 && < 5\n                   , wai-websockets\n                   , websockets\n                   , warp\n                   , wai\n                   , wai-app-static\n                   , bytestring\n                   , case-insensitive\n                   , transformers\n                   , network\n                   , text\n                   , file-embed\n                   , http-types\n  else\n    buildable: False\n\n  ghc-options:       -Wall -threaded\n  main-is:           server.lhs\n\nsource-repository head\n  type:     git\n  location: git://github.com/yesodweb/wai.git\n";
    }