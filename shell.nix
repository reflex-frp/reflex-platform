{ system ? null }:

let
  nixpkgs = import ./deps { inherit system; };

  hsPkgSet = [ { haskellPackages = nixpkgs.reflexPackages.ghcjs;
                 platform = "ghcjs";
               }]
             ++ (if !nixpkgs.stdenv.isDarwin
                 then [ { haskellPackages = nixpkgs.reflexPackages.ghc7101;
                          platform =  "ghc"; } ]
                 else []);

  versions = builtins.map (args: import ./. ({ inherit nixpkgs; } // args)) hsPkgSet;

in nixpkgs.runCommand "shell" {
  buildCommand = ''
    echo "$propagatedBuildInputs $buildInputs $nativeBuildInputs $propagatedNativeBuildInputs" > $out
  '';
  buildInputs = [
    nixpkgs.nodejs
    nixpkgs.curl
    nixpkgs.haskellPackages.cabal-install
  ] ++ nixpkgs.stdenv.lib.concatMap (p: p.env.nativeBuildInputs) versions;
} ""
