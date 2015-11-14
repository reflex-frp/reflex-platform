{ system ? null }:
let this = import ./. { inherit system; };
    reflexEnv = platform: (builtins.getAttr platform this).ghcWithPackages (p: import ./packages.nix { haskellPackages = p; inherit platform; });
in this.nixpkgs.runCommand "shell" {
  buildCommand = ''
    echo "$propagatedBuildInputs $buildInputs $nativeBuildInputs $propagatedNativeBuildInputs" > $out
  '';
  buildInputs = [
    this.nixpkgs.nodejs
    this.nixpkgs.curl
    this.ghc.cabal-install
    this.ghc.ghcid
    this.ghc.cabal2nix
  ] ++ builtins.map reflexEnv this.platforms;
  shellHook = ''
    export PS1="\n\[\033[1;32m\][try-reflex:\w]$\[\033[0m\] "
  '';
} ""
