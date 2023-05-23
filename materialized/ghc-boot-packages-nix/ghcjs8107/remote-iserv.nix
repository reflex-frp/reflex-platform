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
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "remote-iserv"; version = "8.10.7"; };
      license = "BSD-3-Clause";
      copyright = "XXX";
      maintainer = "Moritz Angermann <moritz.angermann@gmail.com>";
      author = "Moritz Angermann <moritz.angermann@gmail.com>";
      homepage = "";
      url = "";
      synopsis = "iserv allows GHC to delegate Tempalte Haskell computations";
      description = "This is a very simple remote runner for iserv, to be used together\nwith iserv-proxy.  The foundamental idea is that this this wrapper\nstarts running libiserv on a given port to which iserv-proxy will\nthen connect.";
      buildType = "Simple";
      };
    components = {
      exes = {
        "remote-iserv" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."libiserv" or (errorHandler.buildDepError "libiserv"))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ./.; }
