with import ./. {};
let inputs = builtins.concatLists [
      (builtins.attrValues sources)
      (map (system: import ./shell.nix { inherit system; }) cacheTargetSystems)
    ];
    stage2Scripts = map (system: (import ./. { inherit system; }).stage2Script) cacheTargetSystems;
in pinBuildInputs "reflex-platform" inputs stage2Scripts
