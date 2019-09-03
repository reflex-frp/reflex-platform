{ haskellLib, nixpkgs, thunkSet }:

self: super: {
  _dep = super._dep or {} // thunkSet ./dep;

  android-activity = self.callPackage self._dep.android-activity {
    inherit (nixpkgs.buildPackages) jdk;
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

  # This should not be necessary. Happy is a native build input, but
  # Nixpkgs splices it to the android version. Haskell splicing
  # appears to be broken! /cc @ericson2314
  haskell-src-exts = haskellLib.overrideCabal super.haskell-src-exts ({
    libraryToolDepends = [ nixpkgs.buildPackages.haskell.packages.ghc844.happy ];
  });

}
