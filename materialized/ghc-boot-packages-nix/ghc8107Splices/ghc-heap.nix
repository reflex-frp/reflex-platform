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
      specVersion = "3.0";
      identifier = { name = "ghc-heap"; version = "8.10.7"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "libraries@haskell.org";
      author = "";
      homepage = "";
      url = "";
      synopsis = "Functions for walking GHC's heap";
      description = "This package provides functions for walking the GHC heap data structures\nand retrieving information about those data structures.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."rts" or (errorHandler.buildDepError "rts"))
          ];
        buildable = true;
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ./.; }
