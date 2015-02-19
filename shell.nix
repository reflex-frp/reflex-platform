{ }:
let this = import ./.;
    reflexEnv = platform: (builtins.getAttr platform this).ghcWithPackages (p: with p; [
      reflex
      reflex-dom
      reflex-todomvc
    ]);
in this.nixpkgs.runCommand "shell" {
  buildCommand = ''
    echo "$propagatedBuildInputs $buildInputs $nativeBuildInputs $propagatedNativeBuildInputs" > $out
  '';
  buildInputs = [
    this.nixpkgs.nodejs
  ] ++ builtins.map reflexEnv this.platforms;
} ""
