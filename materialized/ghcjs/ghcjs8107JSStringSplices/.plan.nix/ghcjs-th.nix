{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = { use-host-template-haskell = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "ghcjs-th"; version = "0.1.0.0"; };
      license = "MIT";
      copyright = "";
      maintainer = "stegeman@gmail.com";
      author = "Luite Stegeman";
      homepage = "http://github.com/ghcjs";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."ghci" or (errorHandler.buildDepError "ghci"))
          ];
        buildable = true;
        modules = [ "GHCJS/Prim/TH/Eval" "GHCJS/Prim/TH/Types" ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../lib/ghcjs-th; }