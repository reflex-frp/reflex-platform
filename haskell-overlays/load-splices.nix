{ haskellLib, fetchFromGitHub, lib, splicedHaskellPackages }:

self: super: {

  # Add some flags to load splices from nativeHaskellPackages
  mkDerivation = drv: super.mkDerivation (drv // {
    configureFlags = let
      attrName = "${drv.pname}_${lib.replaceStrings ["."] ["_"] drv.version}";
      pkg = if builtins.hasAttr drv.pname splicedHaskellPackages
            then builtins.getAttr drv.pname splicedHaskellPackages
            else if builtins.hasAttr attrName splicedHaskellPackages
            then builtins.getAttr attrName splicedHaskellPackages
            else null;
    in (drv.configureFlags or []) ++
      (lib.optionals (pkg != null && pkg ? SPLICE_DIR) [
        "--ghc-option=-load-splices=${pkg}${pkg.SPLICE_DIR}"
      ]);
  });

  haddock = super.haddock.overrideAttrs (drv: {
    patches = (drv.patches or []) ++ [ ./haddock.patch ];
  });

}
