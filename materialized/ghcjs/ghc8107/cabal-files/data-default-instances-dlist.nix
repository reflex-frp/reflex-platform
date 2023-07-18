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
        name = "data-default-instances-dlist";
        version = "0.0.1";
        };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "<l.mai@web.de>";
      author = "Lukas Mai";
      homepage = "";
      url = "";
      synopsis = "Default instances for types in dlist";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/data-default-instances-dlist-0.0.1.tar.gz";
      sha256 = "7d683711cbf08abd7adcd5ac2be825381308d220397315a5570fe61b719b5959";
      });
    }) // {
    package-description-override = "Name:            data-default-instances-dlist\nVersion:         0.0.1\nCabal-Version:   >= 1.6\nCategory:        Data\nSynopsis:        Default instances for types in dlist\nBuild-Type:      Simple\nLicense:         BSD3\nLicense-File:    LICENSE\nAuthor:          Lukas Mai\nMaintainer:      <l.mai@web.de>\n\nsource-repository head\n  type: git\n  location: https://github.com/mauke/data-default\n\nLibrary\n  Build-Depends:     base >=2 && <5, data-default-class, dlist\n  Exposed-Modules:   Data.Default.Instances.DList\n";
    }