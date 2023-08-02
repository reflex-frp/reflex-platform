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
      identifier = { name = "bsb-http-chunked"; version = "0.0.0.4"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2010-2014 Simon Meier\n(c) 2010 Jasper Van der Jeugt\n(c) 2013-2015 Leon P Smith\n(c) 2018 Simon Jakobi";
      maintainer = "Simon Jakobi <simon.jakobi@gmail.com>";
      author = "Jasper Van der Jeugt, Simon Meier, Leon P Smith, Simon Jakobi";
      homepage = "http://github.com/sjakobi/bsb-http-chunked";
      url = "";
      synopsis = "Chunked HTTP transfer encoding for bytestring builders";
      description = "This library contains functions for encoding [bytestring\nbuilders](http://hackage.haskell.org/package/bytestring/docs/Data-ByteString-Builder.html#t:Builder)\nfor [chunked HTTP\\/1.1 transfer](https://en.wikipedia.org/wiki/Chunked_transfer_encoding).\n\nThis functionality was extracted from\nthe [blaze-builder](http://hackage.haskell.org/package/blaze-builder)\npackage.";
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
        "tests" = {
          depends = [
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bsb-http-chunked" or (errorHandler.buildDepError "bsb-http-chunked"))
            (hsPkgs."blaze-builder" or (errorHandler.buildDepError "blaze-builder"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ];
          buildable = true;
          };
        "doctests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."blaze-builder" or (errorHandler.buildDepError "blaze-builder"))
            (hsPkgs."bsb-http-chunked" or (errorHandler.buildDepError "bsb-http-chunked"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/bsb-http-chunked-0.0.0.4.tar.gz";
      sha256 = "148309e23eb8b261c1de374712372d62d8c8dc8ee504c392809c7ec33c0a0e7c";
      });
    }) // {
    package-description-override = "Name:                bsb-http-chunked\r\nVersion:             0.0.0.4\r\nx-revision: 3\r\nSynopsis:            Chunked HTTP transfer encoding for bytestring builders\r\n\r\nDescription:         This library contains functions for encoding [bytestring\r\n                     builders](http://hackage.haskell.org/package/bytestring/docs/Data-ByteString-Builder.html#t:Builder)\r\n                     for [chunked HTTP\\/1.1 transfer](https://en.wikipedia.org/wiki/Chunked_transfer_encoding).\r\n                     .\r\n                     This functionality was extracted from\r\n                     the [blaze-builder](http://hackage.haskell.org/package/blaze-builder)\r\n                     package.\r\n\r\nAuthor:              Jasper Van der Jeugt, Simon Meier, Leon P Smith, Simon Jakobi\r\nCopyright:           (c) 2010-2014 Simon Meier\r\n                     (c) 2010 Jasper Van der Jeugt\r\n                     (c) 2013-2015 Leon P Smith\r\n                     (c) 2018 Simon Jakobi\r\nMaintainer:          Simon Jakobi <simon.jakobi@gmail.com>\r\n\r\nLicense:             BSD3\r\nLicense-file:        LICENSE\r\n\r\nHomepage:            http://github.com/sjakobi/bsb-http-chunked\r\nBug-Reports:         http://github.com/sjakobi/bsb-http-chunked/issues\r\nStability:           Provisional\r\n\r\nCategory:            Data, Network\r\nBuild-type:          Simple\r\nCabal-version:       >= 1.8\r\n\r\nExtra-source-files:  CHANGELOG.md\r\n\r\nSource-repository head\r\n  Type: git\r\n  Location: https://github.com/sjakobi/bsb-http-chunked.git\r\n\r\nLibrary\r\n  exposed-modules:   Data.ByteString.Builder.HTTP.Chunked\r\n  build-depends:     base >= 4.8 && < 5,\r\n                     bytestring >= 0.10.2 && < 0.12\r\n  ghc-options:       -Wall -O2\r\n  if impl(ghc >= 8.0)\r\n    ghc-options:     -Wcompat\r\n\r\ntest-suite tests\r\n  hs-source-dirs: tests\r\n  main-is: Tests.hs\r\n  build-depends:   attoparsec\r\n                 , base\r\n                 , bsb-http-chunked\r\n                 , blaze-builder >= 0.2.1.4\r\n                 , bytestring\r\n                 , hedgehog\r\n                 , tasty\r\n                 , tasty-hedgehog\r\n                 , tasty-hunit\r\n  ghc-options: -Wall -rtsopts\r\n  type: exitcode-stdio-1.0\r\n\r\ntest-suite doctests\r\n  hs-source-dirs: tests\r\n  main-is: Doctests.hs\r\n  build-depends:   base\r\n                 , doctest >= 0.8\r\n  ghc-options: -Wall\r\n  type: exitcode-stdio-1.0\r\n\r\nbenchmark bench\r\n  hs-source-dirs: bench\r\n  main-is: Bench.hs\r\n  build-depends:   base\r\n                 , blaze-builder\r\n                 , bsb-http-chunked\r\n                 , bytestring\r\n                 , deepseq\r\n                 , gauge\r\n                 , semigroups\r\n  ghc-options: -O2 -Wall -rtsopts\r\n  type: exitcode-stdio-1.0\r\n";
    }