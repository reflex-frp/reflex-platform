{}:
with import ./. {};
let inherit (nixpkgs) lib;
    getOtherDeps = reflex-platform: [
      reflex-platform.stage2Script
      reflex-platform.nixpkgs.cabal2nix
      reflex-platform.ghc.cabal2nix
    ] ++ builtins.concatLists (map
      (crossPkgs: lib.optionals (crossPkgs != null) [
        crossPkgs.buildPackages.haskellPackages.cabal2nix
      ]) [
        reflex-platform.nixpkgsCross.ios.aarch64
        reflex-platform.nixpkgsCross.android.aarch64
        reflex-platform.nixpkgsCross.android.aarch32
      ]
    );

in lib.genAttrs cacheBuildSystems (system:
  let
    reflex-platform = (import ./. { inherit system; iosSupportForce = true; });
  in {
    tryReflexShell = reflex-platform.tryReflexShell;
    skeleton-test = import ./skeleton-test.nix { inherit reflex-platform; };
  } // lib.listToAttrs
    (builtins.map (drv: { inherit (drv) name; value = drv; }) (getOtherDeps reflex-platform))
) // {
  benchmark = import ./scripts/benchmark.nix {};
  inherit sources iosReflexTodomvc androidReflexTodomvc;
}
