let local-reflex-platform = import ./. {};
    inherit (local-reflex-platform.nixpkgs) lib;
    getOtherDeps = reflex-platform: [
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

    perPlatform = lib.genAttrs local-reflex-platform.cacheBuildSystems (system:
      let
        reflex-platform = import ./. { inherit system; iosSupportForce = system == "x86_64-darwin"; };
        otherDeps = getOtherDeps reflex-platform;
      in {
        inherit (reflex-platform) sources;
        tryReflexShell = reflex-platform.tryReflexShell;
        ghcjsReflexTodomvc = reflex-platform.ghcjs.reflex-todomvc.overrideAttrs (attrs: {
          postInstall = ''
            ${attrs.postInstall or ""}
            mkdir -p $out/nix-support
            echo $out/bin/reflex-todomvc.jsexe >> $out/nix-support/hydra-build-products
          '';
        });
        ghcReflexTodomvc = reflex-platform.ghc.reflex-todomvc;
        skeleton-test = import ./skeleton-test.nix { inherit reflex-platform; };
        benchmark = import ./scripts/benchmark.nix { inherit reflex-platform; };
        cache = reflex-platform.pinBuildInputs
          "reflex-platform-${system}"
          (builtins.attrValues reflex-platform.sources)
          (otherDeps);
      } // lib.optionalAttrs (system == "x86_64-linux") {
        # The node build is uncached and slow
        benchmark = import ./scripts/benchmark.nix { inherit reflex-platform; };
      } // lib.optionalAttrs (reflex-platform.androidSupport) {
        inherit (reflex-platform) androidReflexTodomvc;
      } // lib.optionalAttrs (reflex-platform.iosSupport) {
        inherit (reflex-platform) iosReflexTodomvc;
      } // lib.listToAttrs
        (builtins.map (drv: { inherit (drv) name; value = drv; }) otherDeps));

    metaCache = local-reflex-platform.pinBuildInputs
      "reflex-platform-everywhere"
      (map (a: a.cache) (builtins.attrValues perPlatform))
      [];

in perPlatform // { inherit metaCache; }
