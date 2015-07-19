{ platform, package }:

let
  localPkgs = import <nixpkgs> {};
in
  with builtins;
  with localPkgs.stdenv.lib.strings;
  with localPkgs.stdenv.lib.lists;
let
  this = import ./deps {};

  haskellPackages = getAttr platform this;

  vanilla = if any (c: c == "/") (stringToCharacters package)
            then haskellPackages.callPackage package {}
            else getAttr package haskellPackages;

  overrider = drv: {
    buildDepends = drv.buildDepends ++ [
      localPkgs.haskellPackages.cabal-install
      localPkgs.haskellPackages.ghcid
    ];
  };

 in (this.nixpkgs.haskell.lib.overrideCabal vanilla overrider).env
