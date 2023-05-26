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
      identifier = { name = "data-default-class"; version = "0.1.2.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "<l.mai@web.de>";
      author = "Lukas Mai";
      homepage = "";
      url = "";
      synopsis = "A class for types with a default value";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && ((compiler.version).ge "7.4" && (compiler.version).lt "7.5")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/data-default-class-0.1.2.0.tar.gz";
      sha256 = "4f01b423f000c3e069aaf52a348564a6536797f31498bb85c3db4bd2d0973e56";
      });
    }) // {
    package-description-override = "Name:            data-default-class\nVersion:         0.1.2.0\nCabal-Version:   >= 1.6\nCategory:        Data\nSynopsis:        A class for types with a default value\nBuild-Type:      Simple\nLicense:         BSD3\nLicense-File:    LICENSE\nAuthor:          Lukas Mai\nMaintainer:      <l.mai@web.de>\n\nsource-repository head\n  type: git\n  location: https://github.com/mauke/data-default\n\nLibrary\n  Build-Depends:     base >=2 && <5\n  if impl(ghc == 7.4.*)\n    -- for GHC.Generics\n    Build-Depends:   ghc-prim\n  Exposed-Modules:   Data.Default.Class\n";
    }