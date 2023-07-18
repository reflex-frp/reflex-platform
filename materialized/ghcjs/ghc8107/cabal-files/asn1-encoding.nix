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
      identifier = { name = "asn1-encoding"; version = "0.9.6"; };
      license = "BSD-3-Clause";
      copyright = "Vincent Hanquez <vincent@snarc.org>";
      maintainer = "vincent@snarc.org";
      author = "Vincent Hanquez <vincent@snarc.org>";
      homepage = "https://github.com/vincenthz/hs-asn1";
      url = "";
      synopsis = "ASN1 data reader and writer in RAW, BER and DER forms";
      description = "ASN1 data reader and writer in raw form with supports for high level forms of ASN1 (BER, and DER).";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."hourglass" or (errorHandler.buildDepError "hourglass"))
          (hsPkgs."asn1-types" or (errorHandler.buildDepError "asn1-types"))
          ];
        buildable = true;
        };
      tests = {
        "tests-asn1-encoding" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."asn1-types" or (errorHandler.buildDepError "asn1-types"))
            (hsPkgs."asn1-encoding" or (errorHandler.buildDepError "asn1-encoding"))
            (hsPkgs."hourglass" or (errorHandler.buildDepError "hourglass"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/asn1-encoding-0.9.6.tar.gz";
      sha256 = "d9f8deabd3b908e5cf83c0d813c08dc0143b3ec1c0d97f660d2cfa02c1c8da0a";
      });
    }) // {
    package-description-override = "Name:                asn1-encoding\r\nVersion:             0.9.6\r\nx-revision: 2\r\nSynopsis:            ASN1 data reader and writer in RAW, BER and DER forms\r\nDescription:\r\n    ASN1 data reader and writer in raw form with supports for high level forms of ASN1 (BER, and DER).\r\nLicense:             BSD3\r\nLicense-file:        LICENSE\r\nCopyright:           Vincent Hanquez <vincent@snarc.org>\r\nAuthor:              Vincent Hanquez <vincent@snarc.org>\r\nMaintainer:          vincent@snarc.org\r\nCategory:            Data\r\nstability:           experimental\r\nBuild-Type:          Simple\r\nCabal-Version:       >=1.10\r\nHomepage:            https://github.com/vincenthz/hs-asn1\r\n\r\nLibrary\r\n  Exposed-modules:   Data.ASN1.Error\r\n                     Data.ASN1.BinaryEncoding\r\n                     Data.ASN1.BinaryEncoding.Raw\r\n                     Data.ASN1.Encoding\r\n                     Data.ASN1.Stream\r\n                     Data.ASN1.Object\r\n                     Data.ASN1.Prim\r\n  other-modules:     Data.ASN1.BinaryEncoding.Parse\r\n                     Data.ASN1.BinaryEncoding.Writer\r\n                     Data.ASN1.Internal\r\n                     Data.ASN1.Serialize\r\n                     Data.ASN1.Get\r\n  Build-Depends:     base >= 4.7 && < 5\r\n                   , bytestring\r\n                   , hourglass >= 0.2.6\r\n                   , asn1-types >= 0.3.0 && < 0.4\r\n  ghc-options:       -Wall -fwarn-tabs\r\n  Default-Language:  Haskell2010\r\n\r\nTest-Suite tests-asn1-encoding\r\n  type:              exitcode-stdio-1.0\r\n  hs-source-dirs:    tests .\r\n  Main-Is:           Tests.hs\r\n  Build-depends:     base >= 3 && < 7\r\n                   , bytestring\r\n                   , mtl\r\n                   , tasty\r\n                   , tasty-quickcheck\r\n                   , asn1-types\r\n                   , asn1-encoding\r\n                   , hourglass\r\n  ghc-options:       -Wall -fno-warn-orphans -fno-warn-missing-signatures\r\n  Default-Language:  Haskell2010\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/vincenthz/hs-asn1\r\n  subdir:   encoding\r\n";
    }