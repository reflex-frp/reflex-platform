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
    flags = { integer-simple = false; integer-gmp = false; };
    package = {
      specVersion = "3.0";
      identifier = { name = "base"; version = "4.14.3.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "libraries@haskell.org";
      author = "";
      homepage = "";
      url = "";
      synopsis = "Basic libraries";
      description = "This package contains the Standard Haskell \"Prelude\" and its support libraries,\nand a large collection of useful libraries ranging from data\nstructures to parsing combinators and debugging utilities.";
      buildType = "Configure";
      };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."rts" or (errorHandler.buildDepError "rts"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          ] ++ (pkgs.lib).optional (!(flags.integer-gmp && !flags.integer-simple || !flags.integer-gmp && flags.integer-simple)) (hsPkgs."invalid-cabal-flag-settings" or (errorHandler.buildDepError "invalid-cabal-flag-settings"))) ++ (pkgs.lib).optional (flags.integer-simple) (hsPkgs."integer-simple" or (errorHandler.buildDepError "integer-simple"))) ++ (pkgs.lib).optional (flags.integer-gmp) (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"));
        libs = (pkgs.lib).optionals (system.isWindows) [
          (pkgs."wsock32" or (errorHandler.sysDepError "wsock32"))
          (pkgs."user32" or (errorHandler.sysDepError "user32"))
          (pkgs."shell32" or (errorHandler.sysDepError "shell32"))
          (pkgs."msvcrt" or (errorHandler.sysDepError "msvcrt"))
          (pkgs."mingw32" or (errorHandler.sysDepError "mingw32"))
          (pkgs."mingwex" or (errorHandler.sysDepError "mingwex"))
          (pkgs."shlwapi" or (errorHandler.sysDepError "shlwapi"))
          ];
        buildable = true;
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ./.; }
