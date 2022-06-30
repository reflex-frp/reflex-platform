{ haskellLib, fetchFromGitHub, lib, ghcVersion }:
let
  isExternalPlugin = lib.versionOlder "8.6" ghcVersion;
  # Add pre configure attributes if our compiler has no support for external plugins
  # e.g. when GHC version <= 8.6
  preConfigureAttrs = attrs: lib.optionalAttrs (!isExternalPlugin)
    {
      preConfigure = ''
        ${attrs.preConfigure or ""}
        configureFlags+=" --ghc-option=-save-splices=$out$SPLICE_DIR"
      '';
    };

  # Add pre build attributes if our compiler has support for external plugins
  # e.g. when GHC version > 8.6
  preBuildAttrs = attrs: lib.optionalAttrs isExternalPlugin
    {
      preBuild = ''
        ${attrs.preBuild or ""}
        echo "!!! save-splices: $out$SPLICE_DIR"
        export EXTERNAL_SPLICES_SAVE=$out$SPLICE_DIR
      '';
    };
in
self: super: {
  mkDerivation = attrs:
    let
      drv = super.mkDerivation (attrs // preConfigureAttrs attrs // preBuildAttrs attrs);

      SPLICE_DIR = "/lib/${drv.compiler.name}/${drv.name}";

    in
    ((drv.overrideAttrs (_: { inherit SPLICE_DIR; })) // { inherit SPLICE_DIR; });

  vector-th-unbox = haskellLib.dontCheck super.vector-th-unbox;
  lens = haskellLib.dontCheck super.lens;

  # Can’t build it outside of android. Hopefully no one wants to use
  # template haskell with it.
  android-activity = null;
}
