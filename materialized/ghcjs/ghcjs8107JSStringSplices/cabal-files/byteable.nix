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
      identifier = { name = "byteable"; version = "0.1.1"; };
      license = "BSD-3-Clause";
      copyright = "Vincent Hanquez <vincent@snarc.org>";
      maintainer = "vincent@snarc.org";
      author = "Vincent Hanquez <vincent@snarc.org>";
      homepage = "http://github.com/vincenthz/hs-byteable";
      url = "";
      synopsis = "Type class for sequence of bytes";
      description = "Abstract class to manipulate sequence of bytes\n\nThe use case of this class is abstracting manipulation of\ntypes that are just wrapping a bytestring with stronger and\nmore meaniful name.\n\nUsual definition of those types are of the form: newtype MyType = MyType ByteString";
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
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/byteable-0.1.1.tar.gz";
      sha256 = "243b34a1b5b64b39e39fe58f75c18f6cad5b668b10cabcd86816cbde27783fe2";
      });
    }) // {
    package-description-override = "Name:                byteable\nVersion:             0.1.1\nSynopsis:            Type class for sequence of bytes\nDescription:\n    Abstract class to manipulate sequence of bytes\n    .\n    The use case of this class is abstracting manipulation of\n    types that are just wrapping a bytestring with stronger and\n    more meaniful name.\n    .\n    Usual definition of those types are of the form: newtype MyType = MyType ByteString\nLicense:             BSD3\nLicense-file:        LICENSE\nCopyright:           Vincent Hanquez <vincent@snarc.org>\nAuthor:              Vincent Hanquez <vincent@snarc.org>\nMaintainer:          vincent@snarc.org\nCategory:            Data\nStability:           experimental\nBuild-Type:          Simple\nHomepage:            http://github.com/vincenthz/hs-byteable\nCabal-Version:       >=1.8\ndata-files:          README.md\n\nLibrary\n  Exposed-modules:   Data.Byteable\n  Build-depends:     base >= 4 && < 5\n                   , bytestring\n  ghc-options:       -Wall -fwarn-tabs\n\nsource-repository head\n  type: git\n  location: git://github.com/vincenthz/hs-byteable\n";
    }