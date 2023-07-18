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
      identifier = { name = "wai"; version = "3.2.3"; };
      license = "MIT";
      copyright = "";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman";
      homepage = "https://github.com/yesodweb/wai";
      url = "";
      synopsis = "Web Application Interface.";
      description = "Provides a common protocol for communication between web applications and web servers.\n\nAPI docs and the README are available at <http://www.stackage.org/package/wai>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."vault" or (errorHandler.buildDepError "vault"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
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
      url = "http://hackage.haskell.org/package/wai-3.2.3.tar.gz";
      sha256 = "5574d6541000988fe204d3032db87fd0a5404cdbde33ee4fa02e6006768229f8";
      });
    }) // {
    package-description-override = "Cabal-Version:       >=1.10\nName:                wai\nVersion:             3.2.3\nSynopsis:            Web Application Interface.\nDescription:         Provides a common protocol for communication between web applications and web servers.\n                     .\n                     API docs and the README are available at <http://www.stackage.org/package/wai>.\nLicense:             MIT\nLicense-file:        LICENSE\nAuthor:              Michael Snoyman\nMaintainer:          michael@snoyman.com\nHomepage:            https://github.com/yesodweb/wai\nCategory:            Web\nBuild-Type:          Simple\nStability:           Stable\nextra-source-files:  README.md ChangeLog.md\n\nSource-repository head\n    type:            git\n    location:        git://github.com/yesodweb/wai.git\n\nLibrary\n  default-language: Haskell2010\n  Build-Depends:     base                      >= 4.10     && < 5\n                   , bytestring                >= 0.10.4\n                   , network                   >= 2.2.1.5\n                   , http-types                >= 0.7\n                   , text                      >= 0.7\n                   , vault                     >= 0.3      && < 0.4\n  Exposed-modules:   Network.Wai\n                     Network.Wai.Internal\n  ghc-options:       -Wall\n\ntest-suite test\n    default-language: Haskell2010\n    hs-source-dirs: test\n    main-is:        Spec.hs\n    type:           exitcode-stdio-1.0\n    ghc-options:    -threaded -Wall\n    cpp-options:    -DTEST\n    build-depends:  base >= 4.8 && < 5\n                  , wai\n                  , hspec\n                  , bytestring\n    other-modules:  Network.WaiSpec\n    build-tool-depends: hspec-discover:hspec-discover\n\nsource-repository head\n  type:     git\n  location: git://github.com/yesodweb/wai.git\n";
    }