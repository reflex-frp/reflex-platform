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
      identifier = { name = "resourcet"; version = "1.2.4.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman";
      homepage = "http://github.com/snoyberg/conduit";
      url = "";
      synopsis = "Deterministic allocation and freeing of scarce resources.";
      description = "Hackage documentation generation is not reliable. For up to date documentation, please see: <http://www.stackage.org/package/resourcet>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/resourcet-1.2.4.2.tar.gz";
      sha256 = "17f20842043ad199961a801b6efb1233b9098eb3537f8395844268f6a223eb87";
      });
    }) // {
    package-description-override = "Name:                resourcet\nVersion:             1.2.4.2\nSynopsis:            Deterministic allocation and freeing of scarce resources.\ndescription:         Hackage documentation generation is not reliable. For up to date documentation, please see: <http://www.stackage.org/package/resourcet>.\nLicense:             BSD3\nLicense-file:        LICENSE\nAuthor:              Michael Snoyman\nMaintainer:          michael@snoyman.com\nCategory:            Data, Conduit\nBuild-type:          Simple\nCabal-version:       >=1.10\nHomepage:            http://github.com/snoyberg/conduit\nextra-source-files:  ChangeLog.md, README.md\n\nLibrary\n  default-language:    Haskell2010\n  Exposed-modules:     Control.Monad.Trans.Resource\n                       Control.Monad.Trans.Resource.Internal\n                       Data.Acquire\n                       Data.Acquire.Internal\n                       UnliftIO.Resource\n  Build-depends:       base                     >= 4.9          && < 5\n                     , containers\n                     , transformers             >= 0.4\n                     , mtl                      >= 2.0          && < 2.3\n                     , exceptions               (== 0.8.* || == 0.10.*)\n                     , unliftio-core\n                     , primitive\n  ghc-options:     -Wall\n\ntest-suite test\n    default-language:    Haskell2010\n    hs-source-dirs: test\n    main-is: main.hs\n    type: exitcode-stdio-1.0\n    cpp-options:   -DTEST\n    build-depends:   resourcet\n                   , base\n                   , exceptions\n                   , hspec >= 1.3\n                   , transformers\n    ghc-options:     -Wall\n\nsource-repository head\n  type:     git\n  location: git://github.com/snoyberg/conduit.git\n";
    }