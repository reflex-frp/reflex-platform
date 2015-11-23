{ haskellPackages, platform }:

with haskellPackages;
let reflexPackages = [ reflex reflex-dom reflex-todomvc ];
in reflexPackages ++
[
  ######################################################################
  # Add general packages here
  ######################################################################

] ++ (if platform == "ghcjs" then [
  ######################################################################
  # Add ghcjs-only packages here
  ######################################################################

] else []) ++ (if platform == "ghc" then [
  ######################################################################
  # Add ghc-only packages here
  ######################################################################

] else []) ++
builtins.concatLists (map (pkg: pkg.override { mkDerivation = drv: drv.buildDepends; }) reflexPackages)
