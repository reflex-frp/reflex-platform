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

  in (drv.overrideAttrs (_: { inherit SPLICE_DIR; }))
     // { inherit SPLICE_DIR; };

  haddock = super.haddock.overrideAttrs (drv: {
    patches = (drv.patches or []) ++ [ ./haddock.patch ];
  });

  vector-th-unbox = haskellLib.dontCheck super.vector-th-unbox;

}
