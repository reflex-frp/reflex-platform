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
        name = "data-default-instances-old-locale";
        version = "0.0.1";
        };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "<l.mai@web.de>";
      author = "Lukas Mai";
      homepage = "";
      url = "";
      synopsis = "Default instances for types in old-locale";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/data-default-instances-old-locale-0.0.1.tar.gz";
      sha256 = "60d3b02922958c4908d7bf2b24ddf61511665745f784227d206745784b0c0802";
      });
    }) // {
    package-description-override = "Name:            data-default-instances-old-locale\nVersion:         0.0.1\nCabal-Version:   >= 1.6\nCategory:        Data\nSynopsis:        Default instances for types in old-locale\nBuild-Type:      Simple\nLicense:         BSD3\nLicense-File:    LICENSE\nAuthor:          Lukas Mai\nMaintainer:      <l.mai@web.de>\n\nsource-repository head\n  type: git\n  location: https://github.com/mauke/data-default\n\nLibrary\n  Build-Depends:     base >=2 && <5, data-default-class, old-locale\n  Exposed-Modules:   Data.Default.Instances.OldLocale\n";
    }