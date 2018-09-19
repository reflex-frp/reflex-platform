let local-reflex-platform = import ./. {};
    inherit (local-reflex-platform.nixpkgs) lib;
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

in lib.genAttrs local-reflex-platform.cacheBuildSystems (system:
  let
    reflex-platform = (import ./. { inherit system; iosSupportForce = system == "x86_64-darwin"; });
  in {
    tryReflexShell = reflex-platform.tryReflexShell;
    skeleton-test = import ./skeleton-test.nix { inherit reflex-platform; };
  } // lib.optionalAttrs (system == "x86_64-linux") {
    # The node build is uncached and slow
    benchmark = import ./scripts/benchmark.nix { inherit reflex-platform; };
  } // lib.optionalAttrs (reflex-platform.androidSupport) {
    inherit (reflex-platform) androidReflexTodomvc;
  } // lib.optionalAttrs (reflex-platform.iosSupport) {
    inherit (reflex-platform) iosReflexTodomvc;
  } // lib.listToAttrs
    (builtins.map (drv: { inherit (drv) name; value = drv; }) (getOtherDeps reflex-platform))
  )
