with import ./.. {};
let inherit (nixpkgs.lib) optionals;
    inputs = builtins.concatLists [
      (builtins.attrValues sources)
      (map (system: (import ./.. { inherit system; iosSupportForce = true; }).cachePackages) cacheTargetSystems)
    ];
    getOtherDeps = reflexPlatform: [
      reflexPlatform.stage2Script
      reflexPlatform.nixpkgs.cabal2nix
    ] ++ builtins.concatLists (map
      (crossPkgs: optionals (crossPkgs != null) [
        crossPkgs.buildPackages.haskellPackages.cabal2nix
      ]) [
        reflexPlatform.nixpkgsCross.ios.arm64
        reflexPlatform.nixpkgsCross.android.arm64Impure
        reflexPlatform.nixpkgsCross.android.armv7aImpure
      ]
    );
    otherDeps = builtins.concatLists (
      map (system: getOtherDeps (import ./.. { inherit system; })) cacheTargetSystems
    ) ++ [(import ./benchmark.nix {})];
in pinBuildInputs "reflex-platform" inputs otherDeps
