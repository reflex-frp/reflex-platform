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
      identifier = { name = "base64-bytestring"; version = "1.2.1.0"; };
      license = "BSD-3-Clause";
      copyright = "2010-2020 Bryan O'Sullivan et al.";
      maintainer = "Herbert Valerio Riedel <hvr@gnu.org>,\nMikhail Glushenkov <mikhail.glushenkov@gmail.com>,\nEmily Pillmore <emilypi@cohomolo.gy>";
      author = "Bryan O'Sullivan <bos@serpentine.com>";
      homepage = "https://github.com/haskell/base64-bytestring";
      url = "";
      synopsis = "Fast base64 encoding and decoding for ByteStrings";
      description = "This package provides support for encoding and decoding binary data according to @base64@ (see also <https://tools.ietf.org/html/rfc4648 RFC 4648>) for strict and lazy ByteStrings\n\nFor a fuller-featured and better-performing Base64 library, see the <https://hackage.haskell.org/package/base64 base64> package.";
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
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "benchmarks" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/base64-bytestring-1.2.1.0.tar.gz";
      sha256 = "fbf8ed30edde271eb605352021431d8f1b055f95a56af31fe2eacf6bdfdc49c9";
      });
    }) // {
    package-description-override = "cabal-version:      1.12\nname:               base64-bytestring\nversion:            1.2.1.0\nsynopsis:           Fast base64 encoding and decoding for ByteStrings\ndescription:\n  This package provides support for encoding and decoding binary data according to @base64@ (see also <https://tools.ietf.org/html/rfc4648 RFC 4648>) for strict and lazy ByteStrings\n  .\n  For a fuller-featured and better-performing Base64 library, see the <https://hackage.haskell.org/package/base64 base64> package.\n\nhomepage:           https://github.com/haskell/base64-bytestring\nbug-reports:        https://github.com/haskell/base64-bytestring/issues\nlicense:            BSD3\nlicense-file:       LICENSE\nauthor:             Bryan O'Sullivan <bos@serpentine.com>\nmaintainer:\n  Herbert Valerio Riedel <hvr@gnu.org>,\n  Mikhail Glushenkov <mikhail.glushenkov@gmail.com>,\n  Emily Pillmore <emilypi@cohomolo.gy>\n\ncopyright:          2010-2020 Bryan O'Sullivan et al.\ncategory:           Data\nbuild-type:         Simple\ntested-with:\n  GHC ==7.0.4\n   || ==7.2.2\n   || ==7.4.2\n   || ==7.6.3\n   || ==7.8.4\n   || ==7.10.3\n   || ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.5\n\nextra-source-files:\n  README.md\n  CHANGELOG.md\n  utils/Transcode.hs\n  utils/transcode.py\n\nlibrary\n  exposed-modules:\n    Data.ByteString.Base64\n    Data.ByteString.Base64.Lazy\n    Data.ByteString.Base64.URL\n    Data.ByteString.Base64.URL.Lazy\n\n  other-modules:    Data.ByteString.Base64.Internal\n  build-depends:\n      base        >=4   && <5\n    , bytestring  >=0.9 && <0.12\n\n  ghc-options:      -Wall -funbox-strict-fields\n  default-language: Haskell2010\n\ntest-suite test\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   tests\n  main-is:          Tests.hs\n  ghc-options:      -Wall -threaded -rtsopts\n  build-depends:\n      base\n    , base64-bytestring\n    , bytestring\n    , HUnit\n    , QuickCheck\n    , test-framework\n    , test-framework-hunit\n    , test-framework-quickcheck2\n\n  default-language: Haskell2010\n\nbenchmark benchmarks\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   benchmarks\n  main-is:          BM.hs\n  ghc-options:      -Wall -threaded -rtsopts\n  build-depends:\n      base\n    , base64-bytestring\n    , bytestring\n    , criterion\n    , deepseq >=1.1\n\n  default-language: Haskell2010\n\nsource-repository head\n  type:     git\n  location: git://github.com/haskell/base64-bytestring\n";
    }