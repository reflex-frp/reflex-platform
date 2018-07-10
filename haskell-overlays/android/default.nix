{ haskellLib, jdk, androidActivity, nativeHaskellPackages, nativeGhc, lib }:

self: super: {
  ghc = super.ghc // {
    bootPkgs = super.ghc.bootPkgs.override {
      overrides = self: super: {
        Cabal = haskellLib.appendPatch (self.callHackage "Cabal" "2.0.0.2" {}) ./Cabal-Allow-any-arch-with-linux-for-foreign-libs.patch;
      };
    };
  };
  android-activity = self.callPackage androidActivity {
    inherit jdk;
  };

  syb = haskellLib.overrideCabal super.syb (drv: { jailbreak = true; });
  cabal-doctest = null;

  # Break version bounds on base for GHC HEAD.
  lifted-async = haskellLib.doJailbreak super.lifted-async;
  safe-exceptions = haskellLib.doJailbreak super.safe-exceptions;

  blaze-textual = haskellLib.enableCabalFlag super.blaze-textual "integer-simple";

  mkDerivation = drv: super.mkDerivation (drv // {
    doHaddock = false;
    dontStrip = true;
    enableSharedExecutables = false;
    configureFlags = let
      nativeDrv = nativeHaskellPackages.${drv.pname} or null;
    in (drv.configureFlags or []) ++ (lib.optionals (nativeDrv != null) [
      "--ghc-option=-ddump-splices"
      "--ghc-option=-load-splices=${nativeDrv}/lib/${nativeGhc.name}/${drv.pname}-${drv.version}"
    ]);
  });

  # HACK(matthewbauer):
  # Temporary fix for https://github.com/ekmett/free/issues/176
  # Optimizations are broken on some ARM-based systems for some reason.
  free = haskellLib.appendConfigureFlag super.free "--enable-optimization=0";
  jsaddle = haskellLib.appendConfigureFlag super.jsaddle "--enable-optimization=0";

  # Disabled for now (jsaddle-wkwebview will probably be better on iOS)
  jsaddle-warp = null;
  # Disable these because these on iOS
  jsaddle-webkitgtk = null;
  jsaddle-webkit2gtk = null;
}
