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
      identifier = { name = "appar"; version = "0.1.8"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Kazu Yamamoto <kazu@iij.ad.jp>";
      author = "Kazu Yamamoto <kazu@iij.ad.jp>";
      homepage = "";
      url = "";
      synopsis = "A simple applicative parser";
      description = "A simple applicative parser in Parsec style";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."fail" or (errorHandler.buildDepError "fail"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/appar-0.1.8.tar.gz";
      sha256 = "c4ceeddc26525b58d82c41b6d3e32141371a200a6794aae185b6266ccc81631f";
      });
    }) // {
    package-description-override = "Name:                   appar\nVersion:                0.1.8\nAuthor:                 Kazu Yamamoto <kazu@iij.ad.jp>\nMaintainer:             Kazu Yamamoto <kazu@iij.ad.jp>\nLicense:                BSD3\nLicense-File:           LICENSE\nSynopsis:               A simple applicative parser\nDescription:            A simple applicative parser in Parsec style\nCategory:               Parsing\nCabal-Version:          >= 1.6\nBuild-Type:             Simple\nExtra-Source-Files:     README\nlibrary\n  GHC-Options:          -Wall\n  Exposed-Modules:      Text.Appar.String\n                        Text.Appar.ByteString\n                        Text.Appar.LazyByteString\n  Other-Modules:        Text.Appar.Input\n                        Text.Appar.Parser\n  Build-Depends:        base >= 4 && < 5, bytestring\n  if impl(ghc >= 8.0)\n    GHC-Options:        -Wcompat -Wnoncanonical-monad-instances -Wnoncanonical-monadfail-instances\n  else\n    Build-Depends:      fail == 4.9.*\nSource-Repository head\n  Type:                 git\n  Location:             git://github.com/kazu-yamamoto/appar.git\n";
    }