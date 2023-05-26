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
      specVersion = "1.12";
      identifier = { name = "unliftio-core"; version = "0.2.0.1"; };
      license = "MIT";
      copyright = "2017-2020 FP Complete";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman, Francesco Mazzoli";
      homepage = "https://github.com/fpco/unliftio/tree/master/unliftio-core#readme";
      url = "";
      synopsis = "The MonadUnliftIO typeclass for unlifting monads to IO";
      description = "Please see the documentation and README at <https://www.stackage.org/package/unliftio-core>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/unliftio-core-0.2.0.1.tar.gz";
      sha256 = "919f0d1297ea2f5373118553c1df2a9405d8b9e31a8307e829da67d4953c299a";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\r\n\r\n-- This file has been generated from package.yaml by hpack version 0.33.0.\r\n--\r\n-- see: https://github.com/sol/hpack\r\n--\r\n-- hash: 9cae5ca1af8760786d8e586fd9b1ed7e329f13f4ec8a3d0aee62818b25038c1f\r\n\r\nname:           unliftio-core\r\nversion:        0.2.0.1\r\nx-revision: 1\r\nsynopsis:       The MonadUnliftIO typeclass for unlifting monads to IO\r\ndescription:    Please see the documentation and README at <https://www.stackage.org/package/unliftio-core>\r\ncategory:       Control\r\nhomepage:       https://github.com/fpco/unliftio/tree/master/unliftio-core#readme\r\nauthor:         Michael Snoyman, Francesco Mazzoli\r\nmaintainer:     michael@snoyman.com\r\ncopyright:      2017-2020 FP Complete\r\nlicense:        MIT\r\nlicense-file:   LICENSE\r\nbuild-type:     Simple\r\nextra-source-files:\r\n    README.md\r\n    ChangeLog.md\r\n\r\nlibrary\r\n  exposed-modules:\r\n      Control.Monad.IO.Unlift\r\n  other-modules:\r\n      Paths_unliftio_core\r\n  hs-source-dirs:\r\n      src\r\n  build-depends:\r\n      base >=4.5 && < 10\r\n    , transformers >=0.2 && <0.6\r\n  default-language: Haskell2010\r\n";
    }