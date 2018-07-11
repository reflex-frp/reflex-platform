{ haskellLib, fetchFromGitHub, ghc, lib }:

self: super: {
  mkDerivation = drv: super.mkDerivation (drv // {

    # ANN is unimplemented in splices.patch
    postPatch = ''
      ${drv.postPatch or ""}
      find . -name '*.hs' -exec sed -i 's/^{-# ANN .* #-}$//' '{}' \;
    '';

    preConfigure = ''
      ${drv.preConfigure or ""}
      spliceDir="$out/lib/${ghc.name}/${drv.pname}-${drv.version}"
      configureFlags+=" --ghc-option=-save-splices=$spliceDir"
    '';

    # Disable a few things that are broken wtih splices.patch
    hyperlinkSource = false;
  });
}
