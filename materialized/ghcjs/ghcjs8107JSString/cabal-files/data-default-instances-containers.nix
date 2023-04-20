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
      identifier = {
        name = "data-default-instances-containers";
        version = "0.0.1";
        };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "<l.mai@web.de>";
      author = "Lukas Mai";
      homepage = "";
      url = "";
      synopsis = "Default instances for types in containers";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/data-default-instances-containers-0.0.1.tar.gz";
      sha256 = "a55e07af005c9815d82f3fc95e125db82994377c9f4a769428878701d4ec081a";
      });
    }) // {
    package-description-override = "Name:            data-default-instances-containers\nVersion:         0.0.1\nCabal-Version:   >= 1.6\nCategory:        Data\nSynopsis:        Default instances for types in containers\nBuild-Type:      Simple\nLicense:         BSD3\nLicense-File:    LICENSE\nAuthor:          Lukas Mai\nMaintainer:      <l.mai@web.de>\n\nsource-repository head\n  type: git\n  location: https://github.com/mauke/data-default\n\nLibrary\n  Build-Depends:     base >=2 && <5, data-default-class, containers\n  Exposed-Modules:   Data.Default.Instances.Containers\n";
    }