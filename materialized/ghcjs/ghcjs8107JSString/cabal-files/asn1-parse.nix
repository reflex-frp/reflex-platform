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
      identifier = { name = "asn1-parse"; version = "0.9.5"; };
      license = "BSD-3-Clause";
      copyright = "Vincent Hanquez <vincent@snarc.org>";
      maintainer = "Vincent Hanquez <vincent@snarc.org>";
      author = "Vincent Hanquez <vincent@snarc.org>";
      homepage = "https://github.com/vincenthz/hs-asn1";
      url = "";
      synopsis = "Simple monadic parser for ASN1 stream types.";
      description = "Simple monadic parser for ASN1 stream types, when ASN1 pattern matching is not convenient.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."asn1-types" or (errorHandler.buildDepError "asn1-types"))
          (hsPkgs."asn1-encoding" or (errorHandler.buildDepError "asn1-encoding"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/asn1-parse-0.9.5.tar.gz";
      sha256 = "8f1fe1344d30b39dc594d74df2c55209577722af1497204b4c2b6d6e8747f39e";
      });
    }) // {
    package-description-override = "Name:                asn1-parse\nVersion:             0.9.5\nDescription:         Simple monadic parser for ASN1 stream types, when ASN1 pattern matching is not convenient.\nLicense:             BSD3\nLicense-file:        LICENSE\nCopyright:           Vincent Hanquez <vincent@snarc.org>\nAuthor:              Vincent Hanquez <vincent@snarc.org>\nMaintainer:          Vincent Hanquez <vincent@snarc.org>\nSynopsis:            Simple monadic parser for ASN1 stream types.\nBuild-Type:          Simple\nCategory:            Data\nstability:           experimental\nCabal-Version:       >=1.6\nHomepage:            https://github.com/vincenthz/hs-asn1\n\nLibrary\n  Build-Depends:     base >= 3 && < 5\n                   , bytestring\n                   , asn1-types >= 0.3 && < 0.4\n                   , asn1-encoding >= 0.9\n  Exposed-modules:   Data.ASN1.Parse\n  ghc-options:       -Wall\n\nsource-repository head\n  type:     git\n  location: https://github.com/vincenthz/hs-asn1\n  subdir:   parse\n";
    }