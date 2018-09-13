{ haskellLib, fetchFromGitHub, lib, splicedHaskellPackages }:

let splicedPkg = drv:
      if builtins.hasAttr drv.pname splicedHaskellPackages
      then builtins.getAttr drv.pname splicedHaskellPackages
      else if builtins.hasAttr (attrName drv) splicedHaskellPackages
      then builtins.getAttr (attrName drv) splicedHaskellPackages
      else throw "no spliced pkg for: ${drv.name}";

    hasSplicedPkg = drv:
      (builtins.hasAttr drv.pname splicedHaskellPackages ||
        builtins.hasAttr (attrName drv) splicedHaskellPackages) &&
      !(builtins.elem drv.pname nonHsPkgs);

    # splicedPkg returns null for those
    nonHsPkgs = [ "android-activity" ];

    attrName = drv:
      "${drv.pname}_${lib.replaceStrings ["."] ["_"] drv.version}";

    spliceDir = drv: let splicedDrv = splicedPkg drv; in
      if splicedDrv == null
      then throw "splicedDrv == null for drv = ${drv.pname}"
      else if splicedDrv.compiler == null
      then throw "spliceDrv.compiler == null"
      else "${splicedDrv}/lib/${splicedDrv.compiler.name}/${splicedDrv.name}";
in
self: super: {

  # Add some flags to load splices from nativeHaskellPackages
  mkDerivation = drv: super.mkDerivation (drv //
  {
    buildFlags = lib.optional (hasSplicedPkg drv) "--ghc-option=-load-splices=${spliceDir drv}"
              ++ (drv.buildFlags or []);
    preBuild = ''
      ${drv.preConfigure or ""}
      echo "!!! has splices: ${if hasSplicedPkg drv then "yes" else "no"}"
      echo "!!! splices at: ${if hasSplicedPkg drv then spliceDir drv else "N/A"} !!!"
    '';
  });

  haddock = super.haddock.overrideAttrs (drv: {
    patches = (drv.patches or []) ++ [ ./haddock.patch ];
  });

}
