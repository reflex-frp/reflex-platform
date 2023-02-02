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
      specVersion = "2.0";
      identifier = { name = "integer-gmp"; version = "1.0.3.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "hvr@gnu.org";
      author = "Herbert Valerio Riedel";
      homepage = "";
      url = "";
      synopsis = "Integer library based on GMP";
      description = "This package provides the low-level implementation of the standard\n'Integer' type based on the\n<http://gmplib.org/ GNU Multiple Precision Arithmetic Library (GMP)>.\n\nThis package provides access to the internal representation of\n'Integer' as well as primitive operations with no proper error\nhandling, and should only be used directly with the utmost care.";
      buildType = "Configure";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          ];
        buildable = true;
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ./.; }
