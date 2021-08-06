{ haskellLib, fetchFromGitHub, lib }:

self: super: {
  mkDerivation = attrs: let

    drv = (super.mkDerivation (attrs // {
      preBuild = ''
        ${attrs.preBuild or ""}
        echo "!!! save-splices: $out$SPLICE_DIR"
        export EXTERNAL_SPLICES_SAVE=$out$SPLICE_DIR
      '';
    }));

    SPLICE_DIR = "/lib/${drv.compiler.name}/${drv.name}";

  in ((drv.overrideAttrs (_: { inherit SPLICE_DIR; })) // { inherit SPLICE_DIR; });

  vector-th-unbox = haskellLib.dontCheck super.vector-th-unbox;
  lens = haskellLib.dontCheck super.lens;

  # Can’t build it outside of android. Hopefully no one wants to use
  # template haskell with it.
  android-activity = null;
}
