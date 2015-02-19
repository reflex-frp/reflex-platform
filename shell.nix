{ }:
let this = import ./.;
    reflexEnv = haskellPackages: haskellPackages.ghcWithPackages (p: with p; [
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
