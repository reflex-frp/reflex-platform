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
      identifier = { name = "http-types"; version = "0.12.3"; };
      license = "BSD-3-Clause";
      copyright = "(C) 2011 Aristid Breitkreuz";
      maintainer = "aristidb@googlemail.com";
      author = "Aristid Breitkreuz, Michael Snoyman";
      homepage = "https://github.com/aristidb/http-types";
      url = "";
      synopsis = "Generic HTTP types for Haskell (for both client and server code).";
      description = "Generic HTTP types for Haskell (for both client and server code).";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
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
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/http-types-0.12.3.tar.gz";
      sha256 = "4e8a4a66477459fa436a331c75e46857ec8026283df984d54f90576cd3024016";
      });
    }) // {
    package-description-override = "Name:                http-types\nVersion:             0.12.3\nSynopsis:            Generic HTTP types for Haskell (for both client and server code).\nDescription:         Generic HTTP types for Haskell (for both client and server code).\nHomepage:            https://github.com/aristidb/http-types\nLicense:             BSD3\nLicense-file:        LICENSE\nAuthor:              Aristid Breitkreuz, Michael Snoyman\nMaintainer:          aristidb@googlemail.com\nCopyright:           (C) 2011 Aristid Breitkreuz\nCategory:            Network, Web\nBuild-type:          Simple\nExtra-source-files:  README, CHANGELOG\nCabal-version:       >=1.8\n\nSource-repository this\n  type: git\n  location: https://github.com/aristidb/http-types.git\n  tag: 0.12.3\n\nSource-repository head\n  type: git\n  location: https://github.com/aristidb/http-types.git\n\nLibrary\n  Exposed-modules:     Network.HTTP.Types\n                       Network.HTTP.Types.Header\n                       Network.HTTP.Types.Method\n                       Network.HTTP.Types.QueryLike\n                       Network.HTTP.Types.Status\n                       Network.HTTP.Types.URI\n                       Network.HTTP.Types.Version\n  GHC-Options:         -Wall\n  Build-depends:       base >= 4 && < 5,\n                       bytestring >=0.10.4.0 && <1.0,\n                       array >=0.2 && <0.6,\n                       case-insensitive >=0.2 && <1.3,\n                       text >= 0.11.0.2\n\nTest-suite spec\n  main-is:             Spec.hs\n  hs-source-dirs:      test\n  type:                exitcode-stdio-1.0\n  GHC-Options:         -Wall\n  build-depends:       base,\n                       http-types,\n                       text,\n                       bytestring,\n                       QuickCheck,\n                       quickcheck-instances,\n                       hspec >= 1.3\n\nTest-Suite doctests\n  main-is:             doctests.hs\n  hs-source-dirs:      test\n  type:                exitcode-stdio-1.0\n  ghc-options:         -threaded -Wall\n  build-depends:       base, doctest >= 0.9.3\n";
    }