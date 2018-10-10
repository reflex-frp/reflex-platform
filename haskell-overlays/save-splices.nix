{ haskellLib, fetchFromGitHub, lib }:

self: super: {
  mkDerivation = attrs: let
    drv = (super.mkDerivation (attrs // {
      preConfigure = ''
        ${attrs.preConfigure or ""}
        configureFlags+=" --ghc-option=-save-splices=$out$SPLICE_DIR"
      '';
    }));

    SPLICE_DIR = "/lib/${drv.compiler.name}/${drv.name}";

    # Not all packages can be spliced with our patched ghc.
    shouldSplice = !(builtins.elem attrs.pname [
      "math-functions"
    ]);

  in if shouldSplice
     then ((drv.overrideAttrs (_: { inherit SPLICE_DIR; }))
          // { inherit SPLICE_DIR; })
     else super.mkDerivation attrs;

  haddock = super.haddock.overrideAttrs (drv: {
    patches = (drv.patches or []) ++ [ ./haddock.patch ];
  });

  vector-th-unbox = haskellLib.dontCheck super.vector-th-unbox;
  lens = haskellLib.dontCheck super.lens;

  # Can’t build it outside of android. Hopefully no one wants to use
  # template haskell with it.
  android-activity = null;
}
