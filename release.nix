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

    drvListToAttrs = drvs:
      lib.listToAttrs (map (drv: { inherit (drv) name; value = drv; }) drvs);

    perPlatform = lib.genAttrs local-reflex-platform.cacheBuildSystems (system:
      let
        reflex-platform = import ./. { inherit system; iosSupportForce = system == "x86_64-darwin"; };
        otherDeps = getOtherDeps reflex-platform;
      in {
        inherit (reflex-platform) sources;
        tryReflexShell = reflex-platform.tryReflexShell;
        ghc.ReflexTodomvc = reflex-platform.ghc.reflex-todomvc;
        ghc8_0.reflexTodomvc = reflex-platform.ghc8_0.reflex-todomvc;
        ghc8_2.reflexTodomvc = reflex-platform.ghc8_2.reflex-todomvc;
        skeleton-test = import ./skeleton-test.nix { inherit reflex-platform; };
        cache = reflex-platform.pinBuildInputs
          "reflex-platform-${system}"
          (builtins.attrValues reflex-platform.sources ++ reflex-platform.cachePackages)
          (otherDeps);
      } // lib.optionalAttrs (system == "x86_64-linux") {
        # The node build is uncached and slow
        benchmark = import ./scripts/benchmark.nix { inherit reflex-platform; };
      } // lib.optionalAttrs (reflex-platform.androidSupport) {
        inherit (reflex-platform) androidReflexTodomvc;
      } // lib.optionalAttrs (reflex-platform.iosSupport) {
        inherit (reflex-platform) iosReflexTodomvc;
      } // drvListToAttrs otherDeps
        // drvListToAttrs (lib.filter lib.isDerivation reflex-platform.cachePackages) # TODO no filter
      );

    metaCache = local-reflex-platform.pinBuildInputs
      "reflex-platform-everywhere"
      (map (a: a.cache) (builtins.attrValues perPlatform))
      [];

in perPlatform // { inherit metaCache; }
