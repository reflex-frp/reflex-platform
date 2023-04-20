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
      identifier = { name = "pem"; version = "0.2.4"; };
      license = "BSD-3-Clause";
      copyright = "Vincent Hanquez <vincent@snarc.org>";
      maintainer = "Vincent Hanquez <vincent@snarc.org>";
      author = "Vincent Hanquez <vincent@snarc.org>";
      homepage = "http://github.com/vincenthz/hs-pem";
      url = "";
      synopsis = "Privacy Enhanced Mail (PEM) format reader and writer.";
      description = "Privacy Enhanced Mail (PEM) format reader and writer. long description";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."basement" or (errorHandler.buildDepError "basement"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          ];
        buildable = true;
        };
      tests = {
        "test-pem" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."pem" or (errorHandler.buildDepError "pem"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/pem-0.2.4.tar.gz";
      sha256 = "770c4c1b9cd24b3db7f511f8a48404a0d098999e28573c3743a8a296bb96f8d4";
      });
    }) // {
    package-description-override = "Name:                pem\nVersion:             0.2.4\nSynopsis:            Privacy Enhanced Mail (PEM) format reader and writer.\nDescription:         Privacy Enhanced Mail (PEM) format reader and writer. long description\nLicense:             BSD3\nLicense-file:        LICENSE\nCopyright:           Vincent Hanquez <vincent@snarc.org>\nAuthor:              Vincent Hanquez <vincent@snarc.org>\nMaintainer:          Vincent Hanquez <vincent@snarc.org>\nBuild-Type:          Simple\nCategory:            Data\nstability:           experimental\nCabal-Version:       >=1.8\nHomepage:            http://github.com/vincenthz/hs-pem\nextra-source-files:  Tests/pem.hs\n\nLibrary\n  Build-Depends:     base >= 3 && < 5\n                   , bytestring\n                   , basement\n                   , memory\n  Exposed-modules:   Data.PEM\n  Other-modules:     Data.PEM.Parser\n                     Data.PEM.Writer\n                     Data.PEM.Types\n  ghc-options:       -Wall\n\nTest-Suite test-pem\n    type:            exitcode-stdio-1.0\n    hs-source-dirs:  Tests\n    main-is:         pem.hs\n    build-depends:   base\n                   , bytestring\n                   , test-framework >= 0.3.3\n                   , test-framework-quickcheck2\n                   , test-framework-hunit\n                   , HUnit\n                   , QuickCheck >= 2.4.0.1\n                   , pem\n\nsource-repository head\n  type: git\n  location: git://github.com/vincenthz/hs-pem\n\n";
    }