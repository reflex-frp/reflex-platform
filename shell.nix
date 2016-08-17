{ system ? null }:
let this = import ./. { inherit system; };
    reflexEnv = platform: (builtins.getAttr platform this).ghcWithPackages (p: import ./packages.nix { haskellPackages = p; inherit platform; });
in this.pinBuildInputs "shell" (this.generalDevTools this.ghc ++ builtins.map reflexEnv this.platforms) []
