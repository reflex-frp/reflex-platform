{}:
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

in lib.genAttrs local-reflex-platform.cacheTargetSystems (system:
  let
    reflex-platform = (import ./. { inherit system; });
  in {
    tryReflexShell = reflex-platform.tryReflexShell;
    ghcjsReflexTodomvc = reflex-platform.ghcjs.reflex-todomvc.overrideAttrs (attrs: {
      postInstall = ''
        ${attrs.postInstall or ""}
        mkdir -p $out/nix-support
        echo $out/bin/reflex-todomvc.jsexe >> $out/nix-support/hydra-build-products
      '';
    });
    ghcReflexTodomvc = reflex-platform.ghc.reflex-todomvc;
    skeleton-test = import ./skeleton-test.nix { this = reflex-platform; };
    benchmark = import ./scripts/benchmark.nix { inherit reflex-platform; };
    inherit (reflex-platform) iosReflexTodomvc androidReflexTodomvc;
  } // lib.listToAttrs
    (builtins.map (drv: { inherit (drv) name; value = drv; }) (getOtherDeps reflex-platform))
  )
