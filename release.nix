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
        reflexPlatform.nixpkgsCross.ios.arm64
        reflexPlatform.nixpkgsCross.android.arm64Impure
        reflexPlatform.nixpkgsCross.android.armv7aImpure
      ]
    );

in nixpkgs.lib.genAttrs cacheTargetSystems (system:
  let
    reflexPlatform = (import ./. { inherit system; iosSupportForce = true; });
  in {
    tryReflexShell = reflexPlatform.tryReflexShell;
  } // nixpkgs.lib.listToAttrs
    (builtins.map (drv: { inherit (drv) name; value = drv; }) (getOtherDeps reflexPlatform))
) // {
  benchmark = import ./scripts/benchmark.nix {};
  inherit sources iosReflexTodomvc androidReflexTodomvc;
}
