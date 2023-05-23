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
      identifier = { name = "data-default"; version = "0.7.1.1"; };
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
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."data-default-instances-containers" or (errorHandler.buildDepError "data-default-instances-containers"))
          (hsPkgs."data-default-instances-dlist" or (errorHandler.buildDepError "data-default-instances-dlist"))
          (hsPkgs."data-default-instances-old-locale" or (errorHandler.buildDepError "data-default-instances-old-locale"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/data-default-0.7.1.1.tar.gz";
      sha256 = "b0f95d279cd75cacaa8152a01590dc3460f7134f6840b37052abb3ba3cb2a511";
      });
    }) // {
    package-description-override = "Name:            data-default\nVersion:         0.7.1.1\nCabal-Version:   >= 1.6\nCategory:        Data\nSynopsis:        A class for types with a default value\nBuild-Type:      Simple\nLicense:         BSD3\nLicense-File:    LICENSE\nAuthor:          Lukas Mai\nMaintainer:      <l.mai@web.de>\n\nsource-repository head\n  type: git\n  location: https://github.com/mauke/data-default\n\nLibrary\n  Build-Depends:     base >=2 && <5, data-default-class >=0.1.2.0,\n                     data-default-instances-containers,\n                     data-default-instances-dlist,\n                     data-default-instances-old-locale\n  Exposed-Modules:   Data.Default\n";
    }