{ haskellLib, fetchFromGitHub, ghc, lib }:

self: super: {
  mkDerivation = drv: super.mkDerivation (drv // {

    # ANN is unimplemented in splices.patch
    postPatch = ''
      ${drv.postPatch or ""}
      find . -name '*.hs' -exec sed -i 's/^{-# ANN .* #-}$//' '{}' \;
    '';

    # We need to find the correct directory to dump splices into. This
    # may be possible through cabal? The directory is usually ./. or
    # ./src
    preConfigure = ''
      ${drv.preConfigure or ""}
      spliceDir="$out/lib/${ghc.name}/${drv.pname}-${drv.version}"
      configureFlags+=" --ghc-option=-ddump-splices"
      configureFlags+=" --ghc-option=-save-splices=$spliceDir"
    '';
  });
}
