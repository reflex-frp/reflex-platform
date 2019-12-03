{ self-args ? { config.android_sdk.accept_license = true; }
, local-self ? import ./. self-args
}:

let
  inherit (local-self.nixpkgs) lib;
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

  drvListToAttrs = drvs:
    lib.listToAttrs (map (drv: { inherit (drv) name; value = drv; }) drvs);

  cacheBuildSystems = [
    "x86_64-linux"
    # "i686-linux"
    "x86_64-darwin"
  ];

  perPlatform = lib.genAttrs cacheBuildSystems (system: let
    getRP = args: import ./. ((self-args // { inherit system; }) // args);
    reflex-platform = getRP {};
    reflex-platform-profiled = getRP { enableLibraryProfiling = true; };
    reflex-platform-nojsstring = getRP { useTextJSString = false; };
    otherDeps = getOtherDeps reflex-platform;
    skeleton-test = import ./skeleton-test.nix { inherit reflex-platform; };

    jsexeHydra = exe: exe.overrideAttrs (attrs: {
      postInstall = ''
        ${attrs.postInstall or ""}
        mkdir -p $out/nix-support
        echo $out/bin/reflex-todomvc.jsexe >> $out/nix-support/hydra-build-products
      '';
    });

    benchmark = import ./nix-utils/benchmark { inherit reflex-platform; };
    demoVM = import ./nix-utils/demo-vm { inherit reflex-platform; };

    # TODO do we still need to do these to ensure srcs (only used at build time)
    # make it to the cache? If not, we can just drop this and all the `_dep`
    # attributes in the overlays.

    dep = {}
      // reflex-platform.ghcjs8_6._dep
      // (lib.optionalAttrs reflex-platform.androidSupport reflex-platform.ghcAndroidAarch64._dep)
      // benchmark.dep
      ;
  in {
    inherit dep;
    tryReflexShell = reflex-platform.tryReflexShell;
    ghcjs.reflexTodomvc = jsexeHydra reflex-platform.ghcjs.reflex-todomvc;
    # Doesn't currently build. Removing from CI until fixed.
    ghcjs8_6.reflexTodomvc = jsexeHydra reflex-platform.ghcjs8_6.reflex-todomvc;
    ghc.ReflexTodomvc = reflex-platform.ghc.reflex-todomvc;
    ghc8_6.reflexTodomvc = reflex-platform.ghc8_6.reflex-todomvc;
    profiled = {
      ghc8_6.reflexTodomvc = reflex-platform-profiled.ghc8_6.reflex-todomvc;
    } // lib.optionalAttrs (reflex-platform.androidSupport) {
      inherit (reflex-platform-profiled) androidReflexTodomvc;
      inherit (reflex-platform-profiled) androidReflexTodomvc-8_6;
    } // lib.optionalAttrs (reflex-platform.iosSupport) {
      inherit (reflex-platform-profiled) iosReflexTodomvc;
      inherit (reflex-platform-profiled) iosReflexTodomvc-8_6;
    };
    nojsstring = {
      ghcjs.reflexTodomvc = reflex-platform-nojsstring.ghcjs.reflex-todomvc;
    };
    skeleton-test-ghc = skeleton-test.ghc;
    skeleton-test-ghcjs = skeleton-test.ghcjs;
    inherit benchmark demoVM;
    cache = reflex-platform.pinBuildInputs "reflex-platform-${system}"
      (builtins.attrValues dep ++ reflex-platform.cachePackages ++ otherDeps);
  } // lib.optionalAttrs (reflex-platform.androidSupport) {
    inherit (reflex-platform) androidReflexTodomvc;
    inherit (reflex-platform) androidReflexTodomvc-8_6;
    skeleton-test-project-android = skeleton-test.project.android;
  } // lib.optionalAttrs (reflex-platform.iosSupport) {
    inherit (reflex-platform) iosReflexTodomvc;
    inherit (reflex-platform) iosReflexTodomvc-8_6;
    skeleton-test-project-ios = skeleton-test.project.ios;
  } // drvListToAttrs otherDeps
    // drvListToAttrs (lib.filter lib.isDerivation reflex-platform.cachePackages) # TODO no filter
  );

  metaCache = local-self.pinBuildInputs "reflex-platform-everywhere"
    (map (a: a.cache) (builtins.attrValues perPlatform));

in perPlatform // { inherit metaCache; }
