{ self-args ? {}
, local-self ? import ./. self-args
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
    getRP = args: import ./. ((self-args // { inherit system; }) // args);
    reflex-platform = getRP {};
    reflex-platform-profiled = getRP { enableLibraryProfiling = true; };
    otherDeps = getOtherDeps reflex-platform;
    skeleton-test = import ./skeleton-test.nix { inherit reflex-platform; };

    jsexeHydra = exe: exe.overrideAttrs (attrs: {
      postInstall = ''
        ${attrs.postInstall or ""}
        mkdir -p $out/nix-support
        echo $out/bin/reflex-todomvc.jsexe >> $out/nix-support/hydra-build-products
      '';
    });
  in {
    inherit (reflex-platform) dep;
    tryReflexShell = reflex-platform.tryReflexShell;
    ghcjs.reflexTodomvc = jsexeHydra reflex-platform.ghcjs.reflex-todomvc;
    # Doesn't currently build. Removing from CI until fixed.
    ghcjs8_4.reflexTodomvc = jsexeHydra reflex-platform.ghcjs8_4.reflex-todomvc;
    ghc.ReflexTodomvc = reflex-platform.ghc.reflex-todomvc;
    ghc8_4.reflexTodomvc = reflex-platform.ghc8_4.reflex-todomvc;
    profiled = {
      ghc8_4.reflexTodomvc = reflex-platform-profiled.ghc8_4.reflex-todomvc;
    } // lib.optionalAttrs (reflex-platform.androidSupport) {
      inherit (reflex-platform-profiled) androidReflexTodomvc;
      inherit (reflex-platform-profiled) androidReflexTodomvc-8_4;
      a = reflex-platform-profiled.ghcAndroidAarch64.a;
    } // lib.optionalAttrs (reflex-platform.iosSupport) {
      inherit (reflex-platform-profiled) iosReflexTodomvc;
      inherit (reflex-platform-profiled) iosReflexTodomvc-8_4;
      a = reflex-platform-profiled.ghcIosAarch64.a;
    };
    skeleton-test-ghc = skeleton-test.ghc;
    skeleton-test-ghcjs = skeleton-test.ghcjs;
    benchmark = import ./scripts/benchmark.nix { inherit reflex-platform; };
    cache = reflex-platform.pinBuildInputs
      "reflex-platform-${system}"
      (builtins.attrValues reflex-platform.dep ++ reflex-platform.cachePackages)
      (otherDeps);
  } // lib.optionalAttrs (system == "x86_64-linux") {
    # The node build is uncached and slow
    benchmark = import ./scripts/benchmark.nix { inherit reflex-platform; };
  } // lib.optionalAttrs (reflex-platform.androidSupport) {
    inherit (reflex-platform) androidReflexTodomvc;
    inherit (reflex-platform) androidReflexTodomvc-8_4;
    a = reflex-platform.ghcAndroidAarch64.a;
    skeleton-test-project-android = skeleton-test.project.android;
  } // lib.optionalAttrs (reflex-platform.iosSupport) {
    inherit (reflex-platform) iosReflexTodomvc;
    inherit (reflex-platform) iosReflexTodomvc-8_4;
    a = reflex-platform.ghcIosAarch64.a;
    skeleton-test-project-ios = skeleton-test.project.ios;
  } // drvListToAttrs otherDeps
    // drvListToAttrs (lib.filter lib.isDerivation reflex-platform.cachePackages) # TODO no filter
  );

  metaCache = local-self.pinBuildInputs
    "reflex-platform-everywhere"
    (map (a: a.cache) (builtins.attrValues perPlatform))
    [];

in perPlatform // { inherit metaCache; }
