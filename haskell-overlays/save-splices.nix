{ haskellLib, fetchFromGitHub, lib }:

self: super: {
  mkDerivation = attrs: let
    drv = (super.mkDerivation (attrs // {
      # ANN is unimplemented in splices.patch
      postPatch = ''
        ${attrs.postPatch or ""}
        find . -name '*.hs' -exec sed -i 's/^{-# ANN .* #-}$//' '{}' \;
      '';

      preConfigure = ''
        ${attrs.preConfigure or ""}
        configureFlags+=" --ghc-option=-save-splices=$out$SPLICE_DIR"
      '';
    }));

    SPLICE_DIR = "/lib/${drv.compiler.name}/${drv.name}";

  in (drv.overrideAttrs (_: { inherit SPLICE_DIR; }))
     // { inherit SPLICE_DIR; };

}
