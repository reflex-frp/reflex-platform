{ haskellLib, fetchFromGitHub, lib, nativeHaskellPackages, nativeGhc }:

self: super: {

  # Add some flags to load splices from nativeHaskellPackages
  mkDerivation = drv: super.mkDerivation (drv // {
    configureFlags = let
      attrName = "${drv.pname}_${lib.replaceStrings ["."] ["_"] drv.version}";
    in (drv.configureFlags or []) ++
    (lib.optionals (builtins.hasAttr attrName nativeHaskellPackages) [
      "--ghc-option=-load-splices=${
        builtins.getAttr attrName nativeHaskellPackages
      }/lib/${nativeGhc.name}/${drv.pname}-${drv.version}"
    ]);

    # Disable a few things that are broken wtih splices.patch
    hyperlinkSource = false;
  });

}
