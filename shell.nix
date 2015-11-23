{ system ? null }:
let this = import ./. { inherit system; };
    reflexEnv = platform: (builtins.getAttr platform this).ghcWithPackages
      (pkg: import ./packages.nix {
        haskellPackages = pkg;
        inherit platform;
      });
in with this; nixpkgs.runCommand "shell" {
  buildCommand = ''
    echo "$propagatedBuildInputs $buildInputs $nativeBuildInputs $propagatedNativeBuildInputs" > $out
  '';

  buildInputs = [
    nixpkgs.nodejs
    nixpkgs.curl
    ghc.cabal-install
    ghc.ghcid
    ghc.cabal2nix
  ] ++ builtins.map reflexEnv platforms;

  shellHook = ''
    export PS1="\n\[\033[1;32m\][try-reflex:\w]\n$\[\033[0m\] "
  '';
} ""
