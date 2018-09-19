let local-reflex-platform = import ../. {};
    inherit (local-reflex-platform.nixpkgs) lib;
    inputs = builtins.concatLists [
      (builtins.attrValues local-reflex-platform.sources)
      (map (system: (import ./.. { inherit system; iosSupportForce = true; }).cachePackages)
           local-reflex-platform.cacheBuildSystems)
    ];
    getOtherDeps = reflexPlatform: [
      reflexPlatform.stage2Script
      reflexPlatform.nixpkgs.cabal2nix
    ] ++ builtins.concatLists (map
      (crossPkgs: lib.optionals (crossPkgs != null) [
        crossPkgs.buildPackages.haskellPackages.cabal2nix
      ]) [
        reflexPlatform.nixpkgsCross.ios.aarch64
        reflexPlatform.nixpkgsCross.android.aarch64
        reflexPlatform.nixpkgsCross.android.aarch32
      ]
    );
    otherDeps = builtins.concatLists (
      map (system: getOtherDeps (import ./.. { inherit system; }))
          local-reflex-platform.cacheBuildSystems
    ) ++ [(import ./benchmark.nix {})];
in local-reflex-platform.pinBuildInputs "reflex-platform" inputs otherDeps
