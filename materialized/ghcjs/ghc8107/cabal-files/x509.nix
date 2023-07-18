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
      identifier = { name = "x509"; version = "1.7.5"; };
      license = "BSD-3-Clause";
      copyright = "Vincent Hanquez <vincent@snarc.org>";
      maintainer = "Vincent Hanquez <vincent@snarc.org>";
      author = "Vincent Hanquez <vincent@snarc.org>";
      homepage = "http://github.com/vincenthz/hs-certificate";
      url = "";
      synopsis = "X509 reader and writer";
      description = "X509 reader and writer. please see README";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."hourglass" or (errorHandler.buildDepError "hourglass"))
          (hsPkgs."pem" or (errorHandler.buildDepError "pem"))
          (hsPkgs."asn1-types" or (errorHandler.buildDepError "asn1-types"))
          (hsPkgs."asn1-encoding" or (errorHandler.buildDepError "asn1-encoding"))
          (hsPkgs."asn1-parse" or (errorHandler.buildDepError "asn1-parse"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          ];
        buildable = true;
        };
      tests = {
        "test-x509" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."hourglass" or (errorHandler.buildDepError "hourglass"))
            (hsPkgs."asn1-types" or (errorHandler.buildDepError "asn1-types"))
            (hsPkgs."x509" or (errorHandler.buildDepError "x509"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/x509-1.7.5.tar.gz";
      sha256 = "b1b0fcbb4aa0d749ed2b54710c2ebd6d900cb932108ad14f97640cf4ca60c7c8";
      });
    }) // {
    package-description-override = "Name:                x509\r\nversion:             1.7.5\r\nx-revision: 1\r\nDescription:         X509 reader and writer. please see README\r\nLicense:             BSD3\r\nLicense-file:        LICENSE\r\nCopyright:           Vincent Hanquez <vincent@snarc.org>\r\nAuthor:              Vincent Hanquez <vincent@snarc.org>\r\nMaintainer:          Vincent Hanquez <vincent@snarc.org>\r\nSynopsis:            X509 reader and writer\r\nBuild-Type:          Simple\r\nCategory:            Data\r\nstability:           experimental\r\nHomepage:            http://github.com/vincenthz/hs-certificate\r\nCabal-Version:       >= 1.10\r\n\r\nLibrary\r\n  Default-Language:  Haskell2010\r\n  Build-Depends:     base >= 4.7 && < 5\r\n                   , bytestring\r\n                   , memory\r\n                   , mtl\r\n                   , containers\r\n                   , hourglass\r\n                   , pem >= 0.1\r\n                   , asn1-types >= 0.3.1 && < 0.4\r\n                   , asn1-encoding >= 0.9 && < 0.10\r\n                   , asn1-parse >= 0.9.3 && < 0.10\r\n                   , cryptonite >= 0.24\r\n  Exposed-modules:   Data.X509\r\n                     Data.X509.EC\r\n  Other-modules:     Data.X509.Internal\r\n                     Data.X509.CertificateChain\r\n                     Data.X509.AlgorithmIdentifier\r\n                     Data.X509.DistinguishedName\r\n                     Data.X509.Cert\r\n                     Data.X509.PublicKey\r\n                     Data.X509.PrivateKey\r\n                     Data.X509.Ext\r\n                     Data.X509.ExtensionRaw\r\n                     Data.X509.CRL\r\n                     Data.X509.OID\r\n                     Data.X509.Signed\r\n  ghc-options:       -Wall\r\n\r\nTest-Suite test-x509\r\n  Default-Language:  Haskell2010\r\n  type:              exitcode-stdio-1.0\r\n  hs-source-dirs:    Tests\r\n  Main-is:           Tests.hs\r\n  Build-Depends:     base >= 3 && < 5\r\n                   , bytestring\r\n                   , mtl\r\n                   , tasty\r\n                   , tasty-quickcheck\r\n                   , hourglass\r\n                   , asn1-types\r\n                   , x509\r\n                   , cryptonite\r\n  ghc-options:       -Wall -fno-warn-orphans -fno-warn-missing-signatures\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: git://github.com/vincenthz/hs-certificate\r\n  subdir:   x509\r\n";
    }