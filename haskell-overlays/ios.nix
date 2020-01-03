{ haskellLib, lib }:

self: super: {
  ghcjs-prim = null;
  websockets = null;
  wai = null;
  warp = null;
  wai-app-static = null;

  cabal-doctest = null;
  syb = haskellLib.overrideCabal super.syb (drv: { jailbreak = true; });

  # HACK(matthewbauer):
  # Temporary fix for https://github.com/ekmett/free/issues/176
  # Optimizations are broken on some ARM-based systems for some reason.
  free = haskellLib.appendConfigureFlag super.free "--enable-optimization=0";
  jsaddle = haskellLib.appendConfigureFlag super.jsaddle "--enable-optimization=0";

  blaze-textual = haskellLib.enableCabalFlag super.blaze-textual "integer-simple";
  cryptonite = haskellLib.disableCabalFlag super.cryptonite "integer-gmp";

  reflex-todomvc = haskellLib.overrideCabal super.reflex-todomvc (drv: {
    postFixup = ''
      mkdir $out/reflex-todomvc.app
      cp reflex-todomvc.app/* $out/reflex-todomvc.app/
      cp $out/bin/reflex-todomvc $out/reflex-todomvc.app/
    '';
  });
  mkDerivation = drv: super.mkDerivation (drv // {
    doHaddock = false;
    enableSharedLibraries = false;
    enableSharedExecutables = false;
  });
}
