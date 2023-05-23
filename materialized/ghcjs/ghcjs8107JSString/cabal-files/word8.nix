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
      identifier = { name = "word8"; version = "0.1.3"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Kazu Yamamoto <kazu@iij.ad.jp>";
      author = "Kazu Yamamoto <kazu@iij.ad.jp>";
      homepage = "";
      url = "";
      synopsis = "Word8 library";
      description = "Word8 library to be used with Data.ByteString";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."word8" or (errorHandler.buildDepError "word8"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "criterion" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."word8" or (errorHandler.buildDepError "word8"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/word8-0.1.3.tar.gz";
      sha256 = "2630934c75728bfbf390c1f0206b225507b354f68d4047b06c018a36823b5d8a";
      });
    }) // {
    package-description-override = "Name:                   word8\nVersion:                0.1.3\nAuthor:                 Kazu Yamamoto <kazu@iij.ad.jp>\nMaintainer:             Kazu Yamamoto <kazu@iij.ad.jp>\nLicense:                BSD3\nLicense-File:           LICENSE\nSynopsis:               Word8 library\nDescription:            Word8 library to be used with Data.ByteString\nCategory:               Data\nCabal-Version:          >= 1.10\nBuild-Type:             Simple\n\nLibrary\n  Default-Language:     Haskell2010\n  GHC-Options:          -Wall\n  Exposed-Modules:      Data.Char8\n                        Data.Word8\n  Build-Depends:        base >= 4 && < 5\n\nTest-Suite spec\n  Type:                 exitcode-stdio-1.0\n  Default-Language:     Haskell2010\n  Hs-Source-Dirs:       test\n  Ghc-Options:          -Wall\n  Main-Is:              Spec.hs\n  Other-Modules:        Char8Spec\n                        Word8Spec\n  Build-Depends:        base\n                      , word8\n                      , hspec\n\nBenchmark criterion\n  Type:                 exitcode-stdio-1.0\n  Default-Language:     Haskell2010\n  Hs-Source-Dirs:       bench\n  Ghc-Options:          -Wall\n  Main-Is:              Bench.hs\n  Build-Depends:        base\n                      , bytestring\n                      , criterion\n                      , word8\n\nSource-Repository head\n  Type:                 git\n  Location:             https://github.com/kazu-yamamoto/word8\n";
    }