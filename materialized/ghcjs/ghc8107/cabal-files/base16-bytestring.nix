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
      identifier = { name = "base16-bytestring"; version = "0.1.1.7"; };
      license = "BSD-3-Clause";
      copyright = "Copyright 2011 MailRank, Inc.;\nCopyright 2010-2020 Bryan O'Sullivan et al.";
      maintainer = "Herbert Valerio Riedel <hvr@gnu.org>,\nMikhail Glushenkov <mikhail.glushenkov@gmail.com>,\nEmily Pillmore <emilypi@cohomolo.gy>";
      author = "Bryan O'Sullivan <bos@serpentine.com>";
      homepage = "http://github.com/haskell/base16-bytestring";
      url = "";
      synopsis = "Fast base16 (hex) encoding and decoding for ByteStrings";
      description = "This package provides support for encoding and decoding binary data according\nto @base16@ (see also <https://tools.ietf.org/html/rfc4648 RFC 4648>) for\nstrict (see \"Data.ByteString.Base16\") and lazy @ByteString@s (see \"Data.ByteString.Base16.Lazy\").\n\nSee also the <https://hackage.haskell.org/package/base-encoding base-encoding> package which\nprovides an uniform API providing conversion paths between more binary and textual types.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/base16-bytestring-0.1.1.7.tar.gz";
      sha256 = "525689679d5cc80fa532c1d5cfeace0f62bbb54134fad514f1ba00d0e7fe69ba";
      });
    }) // {
    package-description-override = "cabal-version:       1.12\r\nname:                base16-bytestring\r\nversion:             0.1.1.7\r\nx-revision: 3\r\nsynopsis:            Fast base16 (hex) encoding and decoding for ByteStrings\r\ndescription:         This package provides support for encoding and decoding binary data according\r\n                     to @base16@ (see also <https://tools.ietf.org/html/rfc4648 RFC 4648>) for\r\n                     strict (see \"Data.ByteString.Base16\") and lazy @ByteString@s (see \"Data.ByteString.Base16.Lazy\").\r\n                     .\r\n                     See also the <https://hackage.haskell.org/package/base-encoding base-encoding> package which\r\n                     provides an uniform API providing conversion paths between more binary and textual types.\r\nhomepage:            http://github.com/haskell/base16-bytestring\r\nbug-reports:         http://github.com/haskell/base16-bytestring/issues\r\nlicense:             BSD3\r\nlicense-file:        LICENSE\r\ncopyright:           Copyright 2011 MailRank, Inc.;\r\n                     Copyright 2010-2020 Bryan O'Sullivan et al.\r\nauthor:              Bryan O'Sullivan <bos@serpentine.com>\r\nmaintainer:          Herbert Valerio Riedel <hvr@gnu.org>,\r\n                     Mikhail Glushenkov <mikhail.glushenkov@gmail.com>,\r\n                     Emily Pillmore <emilypi@cohomolo.gy>\r\ncategory:            Data\r\nbuild-type:          Simple\r\nextra-source-files:  README.md CHANGELOG.md\r\ntested-with:         GHC==8.10.1, GHC==8.8.3, GHC==8.6.5,\r\n                     GHC==8.4.4,  GHC==8.2.2, GHC==8.0.2,\r\n                     GHC==7.10.3, GHC==7.8.4, GHC==7.6.3,\r\n                     GHC==7.4.2,  GHC==7.2.2, GHC==7.0.4\r\n\r\nlibrary\r\n  exposed-modules:\r\n    Data.ByteString.Base16\r\n    Data.ByteString.Base16.Lazy\r\n\r\n  build-depends:\r\n    base == 4.*,\r\n    bytestring >= 0.9 && < 0.11,\r\n    ghc-prim\r\n\r\n  ghc-options: -Wall -funbox-strict-fields\r\n  default-language: Haskell2010\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: http://github.com/haskell/base16-bytestring\r\n\r\ntest-suite test\r\n  type: exitcode-stdio-1.0\r\n  hs-source-dirs: tests\r\n  main-is: Tests.hs\r\n  default-language: Haskell2010\r\n  build-depends: base\r\n               , base16-bytestring\r\n               , bytestring\r\n";
    }