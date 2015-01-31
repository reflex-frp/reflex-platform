{ platform ? "ghcjs" }:
with import ./.;
let haskellPackages =
      if platform == "ghcjs" then haskellPackages_ghcjs
      else if platform == "ghc" then haskellPackages_ghc784
      else error (''Unrecognized platform "${platform}"; please use either "ghcjs" or "ghc"'');
in nixpkgs.runCommand "shell" {
  buildInputs = with haskellPackages; [
    ghc (ghc.ghc.parent.cabalInstall or null) nixpkgs.nodejs
    reflex
    reflexDom
    reflexTodomvc
  ];
} ""
