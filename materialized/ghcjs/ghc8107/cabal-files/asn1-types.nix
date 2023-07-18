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
      specVersion = "1.6";
      identifier = { name = "asn1-types"; version = "0.3.4"; };
      license = "BSD-3-Clause";
      copyright = "Vincent Hanquez <vincent@snarc.org>";
      maintainer = "Vincent Hanquez <vincent@snarc.org>";
      author = "Vincent Hanquez <vincent@snarc.org>";
      homepage = "http://github.com/vincenthz/hs-asn1";
      url = "";
      synopsis = "ASN.1 types";
      description = "ASN.1 standard types";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."hourglass" or (errorHandler.buildDepError "hourglass"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/asn1-types-0.3.4.tar.gz";
      sha256 = "78ee92a251379298ca820fa53edbf4b33c539b9fcd887c86f520c30e3b4e21a8";
      });
    }) // {
    package-description-override = "Name:                asn1-types\nVersion:             0.3.4\nDescription:         ASN.1 standard types\nLicense:             BSD3\nLicense-file:        LICENSE\nCopyright:           Vincent Hanquez <vincent@snarc.org>\nAuthor:              Vincent Hanquez <vincent@snarc.org>\nMaintainer:          Vincent Hanquez <vincent@snarc.org>\nSynopsis:            ASN.1 types\nBuild-Type:          Simple\nCategory:            Data\nstability:           experimental\nCabal-Version:       >=1.6\nHomepage:            http://github.com/vincenthz/hs-asn1\n\nLibrary\n  Build-Depends:     base >= 3 && < 5\n                   , bytestring\n                   , memory\n                   , hourglass\n\n  Exposed-modules:   Data.ASN1.BitArray\n                     Data.ASN1.OID\n                     Data.ASN1.Pretty\n                     Data.ASN1.Types\n                     Data.ASN1.Types.String\n                     Data.ASN1.Types.Lowlevel\n  ghc-options:       -Wall\n\nsource-repository head\n  type:     git\n  location: git://github.com/vincenthz/hs-asn1\n";
    }