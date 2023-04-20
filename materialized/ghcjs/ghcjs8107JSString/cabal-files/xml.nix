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
      identifier = { name = "xml"; version = "1.3.14"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2007-2008 Galois Inc.";
      maintainer = "diatchki@galois.com";
      author = "Galois Inc.";
      homepage = "https://github.com/GaloisInc/xml";
      url = "";
      synopsis = "A simple XML library.";
      description = "A simple XML library.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/xml-1.3.14.tar.gz";
      sha256 = "32d1a1a9f21a59176d84697f96ae3a13a0198420e3e4f1c48abbab7d2425013d";
      });
    }) // {
    package-description-override = "Name:            xml\r\nVersion:         1.3.14\r\nx-revision: 2\r\nHomepage:        https://github.com/GaloisInc/xml\r\nSynopsis:        A simple XML library.\r\nDescription:     A simple XML library.\r\nCategory:        Text, XML\r\nLicense:         BSD3\r\nLicense-File:    LICENSE\r\nAuthor:          Galois Inc.\r\nMaintainer:      diatchki@galois.com\r\nCopyright:       (c) 2007-2008 Galois Inc.\r\nBuild-type:      Simple\r\nCabal-version:   >= 1.6\r\n\r\n\r\nlibrary\r\n  Build-depends:   base >= 3 && < 5, bytestring, text\r\n  Ghc-options:     -Wall -O2\r\n  Exposed-modules: Text.XML.Light,\r\n                   Text.XML.Light.Types,\r\n                   Text.XML.Light.Output,\r\n                   Text.XML.Light.Input,\r\n                   Text.XML.Light.Lexer,\r\n                   Text.XML.Light.Proc\r\n                   Text.XML.Light.Cursor\r\n  Extensions:      FlexibleInstances\r\n\r\nsource-repository head\r\n  type:       git\r\n  location:   git://github.com/GaloisInc/xml.git\r\n\r\n";
    }