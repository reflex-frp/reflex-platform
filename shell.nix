{ system ? null }:

let
  nixpkgs = import <nixpkgs> (if system == null then {} else { inherit system; });
  reflexPkgs = import ./deps { inherit system; };

  hsPkgSet = [ { haskellPackages = reflexPkgs.ghcjs; platform = "ghcjs"; } ]
             ++ (if !nixpkgs.stdenv.isDarwin
                 then [ { haskellPackages = reflexPkgs.ghc7101; platform = "ghc"; } ]
                 else [ ]);

  versions = builtins.map (args: import ./. args) hsPkgSet;

in nixpkgs.runCommand "shell" {
  buildCommand = ''
    echo "$propagatedBuildInputs $buildInputs $nativeBuildInputs $propagatedNativeBuildInputs" > $out
  '';
  buildInputs = [
    reflexPkgs.nixpkgs.nodejs # already build node for ghcjs, so might as well use that version
    nixpkgs.curl
    nixpkgs.haskellngPackages.cabal-install # no need for custom deps here
  ] ++ nixpkgs.stdenv.lib.concatMap (p: p.env.nativeBuildInputs) versions;
} ""
