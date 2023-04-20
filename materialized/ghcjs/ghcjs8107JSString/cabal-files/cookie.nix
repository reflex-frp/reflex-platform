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
      specVersion = "1.8";
      identifier = { name = "cookie"; version = "0.4.5"; };
      license = "MIT";
      copyright = "";
      maintainer = "Michael Snoyman <michael@snoyman.com>";
      author = "Michael Snoyman <michael@snoyman.com>";
      homepage = "http://github.com/snoyberg/cookie";
      url = "";
      synopsis = "HTTP cookie parsing and rendering";
      description = "Hackage documentation generation is not reliable. For up to date documentation, please see: <https://www.stackage.org/package/cookie>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cookie" or (errorHandler.buildDepError "cookie"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/cookie-0.4.5.tar.gz";
      sha256 = "707f94d1b31018b91d6a1e9e19ef5413e20d02cab00ad93a5fd7d7b3b46a3583";
      });
    }) // {
    package-description-override = "name:            cookie\nversion:         0.4.5\nlicense:         MIT\nlicense-file:    LICENSE\nauthor:          Michael Snoyman <michael@snoyman.com>\nmaintainer:      Michael Snoyman <michael@snoyman.com>\nsynopsis:        HTTP cookie parsing and rendering\ndescription:     Hackage documentation generation is not reliable. For up to date documentation, please see: <https://www.stackage.org/package/cookie>.\ncategory:        Web, Yesod\nstability:       Stable\ncabal-version:   >= 1.8\nbuild-type:      Simple\nhomepage:        http://github.com/snoyberg/cookie\nextra-source-files: README.md ChangeLog.md\n\nlibrary\n    build-depends:   base                      >= 4        && < 5\n                   , bytestring                >= 0.10.2\n                   , time                      >= 1.5\n                   , text                      >= 1.1\n                   , data-default-class\n                   , deepseq\n    exposed-modules: Web.Cookie\n    ghc-options:     -Wall\n\ntest-suite test\n    hs-source-dirs: test\n    main-is: Spec.hs\n    type: exitcode-stdio-1.0\n    build-depends: base\n                 , HUnit\n                 , QuickCheck\n                 , bytestring >= 0.10.2\n                 , cookie\n                 , tasty\n                 , tasty-hunit\n                 , tasty-quickcheck\n                 , text >= 1.1\n                 , time >= 1.5\n\nsource-repository head\n  type:     git\n  location: git://github.com/snoyberg/cookie.git\n";
    }