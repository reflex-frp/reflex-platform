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
      identifier = { name = "byteorder"; version = "1.0.4"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Antoine Latter <aslatter@gmail.com>";
      author = "Antoine Latter";
      homepage = "http://community.haskell.org/~aslatter/code/byteorder";
      url = "";
      synopsis = "Exposes the native endianness or byte ordering of the system.";
      description = "This package is for working with the native byte-ordering of\nthe system.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/byteorder-1.0.4.tar.gz";
      sha256 = "bd20bbb586947f99c38a4c93d9d0266f49f6fc581767b51ba568f6d5d52d2919";
      });
    }) // {
    package-description-override = "Name:         byteorder\nVersion:      1.0.4\nCabal-Version:  >= 1.6\nSynopsis:      Exposes the native endianness or byte ordering of the system.\nDescription:   This package is for working with the native byte-ordering of\n  the system.\n\nLicense:      BSD3\nLicense-file: LICENSE\nAuthor:       Antoine Latter\nMaintainer:   Antoine Latter <aslatter@gmail.com>\nHomepage: http://community.haskell.org/~aslatter/code/byteorder\nBuild-type: Simple\n\nCategory: System\n\nSource-Repository head\n  type:   darcs\n  location: http://community.haskell.org/~aslatter/code/byteorder/\n\nLibrary\n\n Build-depends: base == 4.*\n Exposed-modules: System.ByteOrder\n";
    }