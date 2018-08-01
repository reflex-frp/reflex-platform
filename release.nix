{}:
with import ./. {};
let inherit (nixpkgs.lib) optionals;
    getOtherDeps = reflexPlatform: [
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

in nixpkgs.lib.genAttrs cacheTargetSystems (system:
  let
    reflexPlatform = (import ./. { inherit system; });
  in {
    tryReflexShell = reflexPlatform.tryReflexShell;
    ghcjsReflexTodomvc = ghcjs.reflex-todomvc.overrideAttrs (attrs: {
      postInstall = ''
        ${attrs.postInstall or ""}
        mkdir -p $out/nix-support
        echo $out/bin/reflex-todomvc.jsexe >> $out/nix-support/hydra-build-products
      '';
    });
    ghcReflexTodomvc = ghc.reflex-todomvc;
  } // nixpkgs.lib.listToAttrs
    (builtins.map (drv: { inherit (drv) name; value = drv; }) (getOtherDeps reflexPlatform))
) // {
  benchmark = import ./scripts/benchmark.nix {};
  inherit iosReflexTodomvc androidReflexTodomvc;
}
