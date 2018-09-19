with import ./.. {};
let inherit (nixpkgs.lib) optionals;
    inputs = builtins.concatLists [
      (builtins.attrValues sources)
      (map (system: (import ./.. { inherit system; iosSupportForce = true; }).cachePackages) cacheBuildSystems)
    ];
    getOtherDeps = reflexPlatform: [
      reflexPlatform.stage2Script
      reflexPlatform.nixpkgs.cabal2nix
    ] ++ builtins.concatLists (map
      (crossPkgs: optionals (crossPkgs != null) [
        crossPkgs.buildPackages.haskellPackages.cabal2nix
      ]) [
        reflexPlatform.nixpkgsCross.ios.aarch64
        reflexPlatform.nixpkgsCross.android.aarch64
        reflexPlatform.nixpkgsCross.android.aarch32
      ]
    );
    otherDeps = builtins.concatLists (
      map (system: getOtherDeps (import ./.. { inherit system; })) cacheBuildSystems
    ) ++ [(import ./benchmark.nix {})];
in pinBuildInputs "reflex-platform" inputs otherDeps
