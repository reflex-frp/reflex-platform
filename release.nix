{}:
with import ./. {};
let inherit (nixpkgs.lib) optionals;
    getOtherDeps = reflexPlatform: [
      reflexPlatform.stage2Script
      reflexPlatform.nixpkgs.cabal2nix
      reflexPlatform.ghc.cabal2nix
    ] ++ builtins.concatLists (map
      (crossPkgs: optionals (crossPkgs != null) [
        crossPkgs.buildPackages.haskellPackages.cabal2nix
      ]) [
        reflexPlatform.nixpkgsCross.ios.aarch64
        reflexPlatform.nixpkgsCross.android.aarch64
        reflexPlatform.nixpkgsCross.android.aarch32
      ]
    );

in nixpkgs.lib.genAttrs cacheBuildSystems (system:
  let
    reflexPlatform = (import ./. { inherit system; iosSupportForce = true; });
  in {
    tryReflexShell = reflexPlatform.tryReflexShell;
    skeleton-test = import ./skeleton-test.nix { this = reflexPlatform; };
  } // nixpkgs.lib.listToAttrs
    (builtins.map (drv: { inherit (drv) name; value = drv; }) (getOtherDeps reflexPlatform))
) // {
  benchmark = import ./scripts/benchmark.nix {};
  inherit sources iosReflexTodomvc androidReflexTodomvc;
}
