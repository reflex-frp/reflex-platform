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
      identifier = { name = "base64-bytestring"; version = "1.0.0.3"; };
      license = "BSD-3-Clause";
      copyright = "2010-2018 Bryan O'Sullivan et al.";
      maintainer = "Herbert Valerio Riedel <hvr@gnu.org>,\nMikhail Glushenkov <mikhail.glushenkov@gmail.com>";
      author = "Bryan O'Sullivan <bos@serpentine.com>";
      homepage = "https://github.com/haskell/base64-bytestring";
      url = "";
      synopsis = "Fast base64 encoding and decoding for ByteStrings";
      description = "This package provides support for encoding and decoding binary data according to @base64@ (see also <https://tools.ietf.org/html/rfc4648 RFC 4648>) for strict and lazy ByteStrings.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."split" or (errorHandler.buildDepError "split"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "benchmarks" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/base64-bytestring-1.0.0.3.tar.gz";
      sha256 = "ef159d60ec14c0a3f3e26bab5c9fd7634d5e1b983c6a64f0b0c3261efe008fc7";
      });
    }) // {
    package-description-override = "name:                base64-bytestring\r\nversion:             1.0.0.3\r\nx-revision: 1\r\nsynopsis:            Fast base64 encoding and decoding for ByteStrings\r\ndescription:         This package provides support for encoding and decoding binary data according to @base64@ (see also <https://tools.ietf.org/html/rfc4648 RFC 4648>) for strict and lazy ByteStrings.\r\nhomepage:            https://github.com/haskell/base64-bytestring\r\nbug-reports:         https://github.com/haskell/base64-bytestring/issues\r\nlicense:             BSD3\r\nlicense-file:        LICENSE\r\nauthor:              Bryan O'Sullivan <bos@serpentine.com>\r\nmaintainer:          Herbert Valerio Riedel <hvr@gnu.org>,\r\n                     Mikhail Glushenkov <mikhail.glushenkov@gmail.com>\r\ncopyright:           2010-2018 Bryan O'Sullivan et al.\r\ncategory:            Data\r\nbuild-type:          Simple\r\ncabal-version:       >=1.8\r\ntested-with:         GHC==8.6.2, GHC==8.4.4,  GHC==8.2.2,\r\n                     GHC==8.0.2, GHC==7.10.3, GHC==7.8.4,\r\n                     GHC==7.6.3, GHC==7.4.2,  GHC==7.2.2,\r\n                     GHC==7.0.4\r\n\r\nextra-source-files:\r\n  README.md\r\n  CHANGELOG.md\r\n  utils/Transcode.hs\r\n  utils/transcode.py\r\n\r\nlibrary\r\n  exposed-modules:\r\n    Data.ByteString.Base64\r\n    Data.ByteString.Base64.URL\r\n    Data.ByteString.Base64.Lazy\r\n    Data.ByteString.Base64.URL.Lazy\r\n\r\n  other-modules:\r\n    Data.ByteString.Base64.Internal\r\n\r\n  build-depends:\r\n    base == 4.*,\r\n    bytestring >= 0.9.0 && < 0.11\r\n\r\n  ghc-options: -Wall -funbox-strict-fields\r\n\r\ntest-suite tests\r\n  type: exitcode-stdio-1.0\r\n  hs-source-dirs: tests\r\n  main-is: Tests.hs\r\n\r\n  ghc-options:\r\n    -Wall -threaded -rtsopts\r\n\r\n  build-depends:\r\n    QuickCheck,\r\n    HUnit,\r\n    base64-bytestring,\r\n    base,\r\n    containers,\r\n    bytestring,\r\n    split,\r\n    test-framework,\r\n    test-framework-quickcheck2,\r\n    test-framework-hunit\r\n\r\nbenchmark benchmarks\r\n  type: exitcode-stdio-1.0\r\n  hs-source-dirs: benchmarks\r\n  main-is: BM.hs\r\n\r\n  ghc-options:\r\n    -Wall -threaded -rtsopts\r\n\r\n  build-depends:\r\n    base,\r\n    bytestring,\r\n    containers,\r\n    deepseq,\r\n    base64-bytestring,\r\n    criterion\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: git://github.com/bos/base64-bytestring\r\n\r\nsource-repository head\r\n  type:     mercurial\r\n  location: https://bitbucket.org/bos/base64-bytestring\r\n";
    }