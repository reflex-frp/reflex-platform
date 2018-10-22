{ local-self ? import ./. {}
}:

let
  inherit (local-self.nixpkgs) lib;
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

  perPlatform = lib.genAttrs local-self.cacheBuildSystems (system: let
    reflex-platform-legacy-compilers = import ./. { inherit system; __useLegacyCompilers = true; };
    reflex-platform = import ./. { inherit system; };
    otherDeps = getOtherDeps reflex-platform;

    jsexeHydra = exe: exe.overrideAttrs (attrs: {
      postInstall = ''
        ${attrs.postInstall or ""}
        mkdir -p $out/nix-support
        echo $out/bin/reflex-todomvc.jsexe >> $out/nix-support/hydra-build-products
      '';
    });
  in {
    inherit (reflex-platform) sources;
    tryReflexShell = reflex-platform.tryReflexShell;
    ghcjs.reflexTodomvc = jsexeHydra reflex-platform.ghcjs.reflex-todomvc;
    ghcjs8_0.reflexTodomvc = jsexeHydra reflex-platform.ghcjs8_0.reflex-todomvc;
    # Doesn't currently build. Removing from CI until fixed.
    # ghcjs8_2.reflexTodomvc = jsexeHydra reflex-platform.ghcjs8_2.reflex-todomvc;
    ghcjs8_4.reflexTodomvc = jsexeHydra reflex-platform.ghcjs8_4.reflex-todomvc;
    ghc.ReflexTodomvc = reflex-platform.ghc.reflex-todomvc;
    ghc8_0.reflexTodomvc = reflex-platform.ghc8_0.reflex-todomvc;
    ghc8_2.reflexTodomvc = reflex-platform.ghc8_2.reflex-todomvc;
    ghc8_4.reflexTodomvc = reflex-platform.ghc8_4.reflex-todomvc;
    skeleton-test = import ./skeleton-test.nix { inherit reflex-platform; };
    # TODO update reflex-project-skeleton to also cover ghc80 instead of using legacy compilers option
    skeleton-test-legacy-compilers = import ./skeleton-test.nix { reflex-platform = reflex-platform-legacy-compilers; };
    benchmark = import ./scripts/benchmark.nix { inherit reflex-platform; };
    cache = reflex-platform.pinBuildInputs
      "reflex-platform-${system}"
      (lib.concatMap builtins.attrValues (builtins.attrValues reflex-platform.sources)
        ++ reflex-platform.cachePackages)
      (otherDeps);
  } // lib.optionalAttrs (system == "x86_64-linux") {
    # The node build is uncached and slow
    benchmark = import ./scripts/benchmark.nix { inherit reflex-platform; };
  } // lib.optionalAttrs (reflex-platform.androidSupport) {
    inherit (reflex-platform) androidReflexTodomvc;
    inherit (reflex-platform) androidReflexTodomvc-8_2;
    inherit (reflex-platform) androidReflexTodomvc-8_4;
    a = reflex-platform.ghcAndroidAarch64.a;
  } // lib.optionalAttrs (reflex-platform.iosSupport) {
    inherit (reflex-platform) iosReflexTodomvc;
    inherit (reflex-platform) iosReflexTodomvc-8_2;
    inherit (reflex-platform) iosReflexTodomvc-8_4;
    a = reflex-platform.ghcIosAarch64.a;
  } // drvListToAttrs otherDeps
    // drvListToAttrs (lib.filter lib.isDerivation reflex-platform.cachePackages) # TODO no filter
  );

  metaCache = local-self.pinBuildInputs
    "reflex-platform-everywhere"
    (map (a: a.cache) (builtins.attrValues perPlatform))
    [];

in perPlatform // { inherit metaCache; }
