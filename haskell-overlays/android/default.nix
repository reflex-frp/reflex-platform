{ haskellLib, nixpkgs, thunkSet }:

let
  # add the "-fPIC" option to both "ghc-options" and "cc-options" for the library component of a package
  enableFPic = pkg: haskellLib.overrideCabal pkg (old: {
    preConfigure = ''
      sed -i 's/^library *\(.*\)$/library \1\n  cc-options: -fPIC\n  ghc-options: -fPIC/i' *.cabal
    '';
  });
in
self: super: {
  _dep = super._dep or {} // thunkSet ./dep;

  android-activity = self.callPackage self._dep.android-activity {
    jdk = nixpkgs.buildPackages.openjdk17_headless;
  };

  syb = haskellLib.overrideCabal super.syb (drv: { jailbreak = true; });
  cabal-doctest = null;

  # Break version bounds on base for GHC HEAD.
  lifted-async = haskellLib.doJailbreak super.lifted-async;
  safe-exceptions = haskellLib.doJailbreak super.safe-exceptions;

  blaze-textual = haskellLib.enableCabalFlag super.blaze-textual "integer-simple";
  cryptonite = haskellLib.disableCabalFlag super.cryptonite "integer-gmp";

  hashable = enableFPic super.hashable;
  primitive = enableFPic super.primitive;

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

}
