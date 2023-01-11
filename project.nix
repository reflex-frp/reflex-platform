{ name, compiler-nix-name ? "ghc8107", src, overrides ? [ ], extraSrcFiles ? [ ] }:
let
  # TODO:
  # - Remove this let box properly
  # - Allow for pkgs to be overriden
  haskell-nix = import ../haskell.nix { };
  pkgs = import haskell-nix.sources.nixpkgs-unstable (haskell-nix.nixpkgsArgs);
in
(pkgs.haskell-nix.project' ({
  inherit name compiler-nix-name;
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    inherit name src;
  };
  modules = [
    { packages."${name}".components = extraSrcFiles; }
    ({ config, lib, ... }: {         
        config.preBuild = ''
          echo "!!! Save Splices $out/lib/haskell.nix/$pname"
          export EXTERNAL_SPLICES_SAVE="$out/lib/haskell.nix/$pname"
        '';
      })
  ] ++ overrides;
})).extend (final: prev: rec {
  # Usage of cross-driver sets up all of the various splices cruft to
  # make an easy way to setup cross-compiling with splices
  crossSystems = builtins.mapAttrs (a: v: import ./modules/cross-driver.nix {
    plan-pkgs = import ("${prev.plan-nix}/default.nix");  
    inherit (pkgs) pkgsCross;
    inherit compiler-nix-name;
    inherit (final) pkg-set;
    #spliced-packages = final.pkg-set;
    crossSystem = toString a;
    splice-driver = import ./modules/splice-driver.nix { dontSplice = [ "fgl" "Cabal" ]; };
    inherit overrides;
  }) pkgs.pkgsCross;
})
