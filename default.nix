{ nixpkgs, haskellPackages, platform }:

haskellPackages.mkDerivation {
  pname = "reflex-demo";
  version = "0.0.0.0";
  license = null;

  buildDepends = import ./packages.nix { inherit haskellPackages platform; };
}
