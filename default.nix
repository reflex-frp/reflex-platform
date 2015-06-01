{ nixpkgs, haskellPackages, platform }:

haskellPackages.mkDerivation {
  pname = "reflex-demo";
  version = "0.0.0.0";
  license = null;

  buildDepends = import ./packages.nix { inherit haskellPackages platform; };
} // {
  # The systems that we want to build for on the current system
  cacheTargetSystems =
    if nixpkgs.stdenv.system == "x86_64-linux"
    then [ "x86_64-linux" "i686-linux" ] # On linux, we want to build both 32-bit and 64-bit versions
    else [ nixpkgs.stdenv.system ];
}
