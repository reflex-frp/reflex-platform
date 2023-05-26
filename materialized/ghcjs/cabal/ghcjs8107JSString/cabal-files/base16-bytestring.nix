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
      identifier = { name = "base16-bytestring"; version = "1.0.2.0"; };
      license = "BSD-3-Clause";
      copyright = "Copyright 2011 MailRank, Inc.;\nCopyright 2010-2020 Bryan O'Sullivan et al.";
      maintainer = "Herbert Valerio Riedel <hvr@gnu.org>,\nMikhail Glushenkov <mikhail.glushenkov@gmail.com>,\nEmily Pillmore <emilypi@cohomolo.gy>";
      author = "Bryan O'Sullivan <bos@serpentine.com>";
      homepage = "http://github.com/haskell/base16-bytestring";
      url = "";
      synopsis = "RFC 4648-compliant Base16 encodings for ByteStrings";
      description = "This package provides support for encoding and decoding binary data according\nto @base16@ (see also <https://tools.ietf.org/html/rfc4648 RFC 4648>) for\nstrict (see \"Data.ByteString.Base16\") and lazy @ByteString@s (see \"Data.ByteString.Base16.Lazy\").\n\nSee the <https://hackage.haskell.org/package/base16 base16> package which provides superior encoding and decoding performance as well as support for lazy, short, and strict variants of 'Text' and 'ByteString' values. Additionally, see the <https://hackage.haskell.org/package/base-encoding base-encoding> package which\nprovides an uniform API providing conversion paths between more binary and textual types.";
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
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
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
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
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
      url = "http://hackage.haskell.org/package/base16-bytestring-1.0.2.0.tar.gz";
      sha256 = "1d5a91143ef0e22157536093ec8e59d226a68220ec89378d5dcaeea86472c784";
      });
    }) // {
    package-description-override = "cabal-version:      1.12\nname:               base16-bytestring\nversion:            1.0.2.0\nsynopsis:           RFC 4648-compliant Base16 encodings for ByteStrings\ndescription:\n  This package provides support for encoding and decoding binary data according\n  to @base16@ (see also <https://tools.ietf.org/html/rfc4648 RFC 4648>) for\n  strict (see \"Data.ByteString.Base16\") and lazy @ByteString@s (see \"Data.ByteString.Base16.Lazy\").\n  .\n  See the <https://hackage.haskell.org/package/base16 base16> package which provides superior encoding and decoding performance as well as support for lazy, short, and strict variants of 'Text' and 'ByteString' values. Additionally, see the <https://hackage.haskell.org/package/base-encoding base-encoding> package which\n  provides an uniform API providing conversion paths between more binary and textual types.\n\nhomepage:           http://github.com/haskell/base16-bytestring\nbug-reports:        http://github.com/haskell/base16-bytestring/issues\nlicense:            BSD3\nlicense-file:       LICENSE\ncopyright:\n  Copyright 2011 MailRank, Inc.;\n  Copyright 2010-2020 Bryan O'Sullivan et al.\n\nauthor:             Bryan O'Sullivan <bos@serpentine.com>\nmaintainer:\n  Herbert Valerio Riedel <hvr@gnu.org>,\n  Mikhail Glushenkov <mikhail.glushenkov@gmail.com>,\n  Emily Pillmore <emilypi@cohomolo.gy>\n\ncategory:           Data\nbuild-type:         Simple\nextra-source-files:\n  README.md\n  CHANGELOG.md\n\ntested-with:\n  GHC ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.4\n   || ==9.0.1\n\nsource-repository head\n  type:     git\n  location: http://github.com/haskell/base16-bytestring\n\nlibrary\n  other-modules:    Data.ByteString.Base16.Internal\n  exposed-modules:\n    Data.ByteString.Base16\n    Data.ByteString.Base16.Lazy\n\n  build-depends:\n      base        >=4.9 && <5\n    , bytestring  >=0.9 && <0.12\n\n  ghc-options:      -Wall -funbox-strict-fields\n  default-language: Haskell2010\n\ntest-suite test\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   tests\n  main-is:          Tests.hs\n  build-depends:\n      base\n    , base16-bytestring\n    , bytestring\n    , HUnit\n    , QuickCheck\n    , test-framework\n    , test-framework-hunit\n    , test-framework-quickcheck2\n\n  default-language: Haskell2010\n\nbenchmark bench\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   benchmarks\n  main-is:          Benchmarks.hs\n  build-depends:\n      base               >=4 && <5\n    , base16-bytestring\n    , bytestring\n    , criterion\n    , deepseq\n\n  default-language: Haskell2010\n";
    }