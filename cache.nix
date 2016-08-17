with import ./. {};
let inputs = builtins.concatLists [
      (builtins.attrValues sources)
      (map (system: import ./shell.nix { inherit system; }) cacheTargetSystems)
    ];
    otherDeps = [
      stage2Script
    ];
in pinBuildInputs "reflex-platform" inputs otherDeps
