{ platform ? "ghcjs" }:
let this = import ./.;
    haskellPackages =
      if platform == "ghcjs" then this.ghcjs
      else if platform == "ghc" then this.ghc
      else builtins.error (''Unrecognized platform "${platform}"; please use either "ghcjs" or "ghc"'');
in this.nixpkgs.runCommand "shell" {
  buildCommand = ''
    echo "$propagatedBuildInputs $buildInputs $nativeBuildInputs $propagatedNativeBuildInputs" > $out
  '';
  buildInputs = [
    (haskellPackages.ghcWithPackages (p: with p; [
      reflex
      reflex-dom
      reflex-todomvc
    ]))
    this.nixpkgs.nodejs
  ];
} ""
