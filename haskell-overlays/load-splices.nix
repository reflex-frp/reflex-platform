{ haskellLib, fetchFromGitHub, lib, nativeHaskellPackages }:

self: super: {

  # Add some flags to load splices from nativeHaskellPackages
  mkDerivation = drv: super.mkDerivation (drv // {
    configureFlags = let
      attrName = "${drv.pname}_${lib.replaceStrings ["."] ["_"] drv.version}";
      pkg = builtins.getAttr attrName nativeHaskellPackages;
    in (drv.configureFlags or []) ++
      (lib.optionals (builtins.hasAttr attrName nativeHaskellPackages) [
        "--ghc-option=-load-splices=${pkg}${pkg.SPLICE_DIR}"
      ]);
  });

}
