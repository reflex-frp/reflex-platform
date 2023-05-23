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
      identifier = { name = "iserv"; version = "8.10.7"; };
      license = "BSD-3-Clause";
      copyright = "XXX";
      maintainer = "XXX";
      author = "XXX";
      homepage = "";
      url = "";
      synopsis = "iserv allows GHC to delegate Template Haskell computations";
      description = "GHC can be provided with a path to the iserv binary with\n@-pgmi=/path/to/iserv-bin@, and will in combination with\n@-fexternal-interpreter@, compile Template Haskell though the\n@iserv-bin@ delegate. This is very similar to how ghcjs has been\ncompiling Template Haskell, by spawning a separate delegate (so\ncalled runner on the javascript vm) and evaluating the splices\nthere.\n\nTo use iserv with cross compilers, please see @libraries/libiserv@\nand @utils/iserv-proxy@.";
      buildType = "Simple";
      };
    components = {
      exes = {
        "iserv" = {
          depends = [
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."ghci" or (errorHandler.buildDepError "ghci"))
            (hsPkgs."libiserv" or (errorHandler.buildDepError "libiserv"))
            ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ./.; }
