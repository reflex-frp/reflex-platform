{ haskellLib, jdk }:

self: super: {
  ghc = super.ghc // {
    bootPkgs = super.ghc.bootPkgs.override {
      overrides = self: super: {
        Cabal = haskellLib.appendPatch (self.callHackage "Cabal" "2.0.0.2" {}) ./Cabal-Allow-any-arch-with-linux-for-foreign-libs.patch;
      };
    };
  };
  android-activity = self.callPackage ../../android/android-activity {
    inherit jdk;
  };

  syb = haskellLib.overrideCabal super.syb (drv: { jailbreak = true; });
  cabal-doctest = null;

  # Break version bounds on base for GHC HEAD.
  lifted-async = haskellLib.doJailbreak super.lifted-async;
  safe-exceptions = haskellLib.doJailbreak super.safe-exceptions;

  mkDerivation = drv: super.mkDerivation (drv // {
    doHaddock = false;
    dontStrip = true;
    enableSharedExecutables = false;
  });
}
