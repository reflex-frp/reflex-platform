{ system ? null }:
let this = import ./. { inherit system; };
    reflexEnv = platform: (builtins.getAttr platform this).ghcWithPackages (p: import ./packages.nix { haskellPackages = p; inherit platform; });
in this.nixpkgs.runCommand "shell" {
  buildCommand = ''
    echo "$propagatedBuildInputs $buildInputs $nativeBuildInputs $propagatedNativeBuildInputs" > $out
  '';
  buildInputs = this.generalDevTools ++ builtins.map reflexEnv this.platforms;
} ""
