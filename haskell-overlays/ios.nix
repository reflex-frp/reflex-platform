{ haskellLib }:

self: super: {
  ghcjs-prim = null;
  ghcjs-json = null;
  derive = null;
  focus-http-th = null;
  th-lift-instances = null;
  websockets = null;
  wai = null;
  warp = null;
  wai-app-static = null;

  cabal-doctest = null;
  syb = haskellLib.overrideCabal super.syb (drv: { jailbreak = true; });

  reflex-todomvc = haskellLib.overrideCabal super.reflex-todomvc (drv: {
    postFixup = ''
      mkdir $out/reflex-todomvc.app
      cp reflex-todomvc.app/* $out/reflex-todomvc.app/
      cp $out/bin/reflex-todomvc $out/reflex-todomvc.app/
    '';
  });
  cabal-macosx = null;
  mkDerivation = drv: super.mkDerivation (drv // {
    doHaddock = false;
    enableSharedLibraries = false;
    enableSharedExecutables = false;
  });
}
