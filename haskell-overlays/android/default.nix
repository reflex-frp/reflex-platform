{ haskellLib, jdk }:

with haskellLib;

self: super: {
  ghc = super.ghc // {
    bootPkgs = super.ghc.bootPkgs.override {
      overrides = self: super: {
        Cabal = appendPatch (self.callHackage "Cabal" "2.0.0.2" {}) ./Cabal-Allow-any-arch-with-linux-for-foreign-libs.patch;
      };
    };
  };
  android-activity = self.callPackage ../../android/android-activity {
    inherit jdk;
  };

  syb = overrideCabal super.syb (drv: { jailbreak = true; });
  cabal-doctest = null;

  # Break version bounds on base for GHC HEAD.
  lifted-async = doJailbreak super.lifted-async;
  safe-exceptions = doJailbreak super.safe-exceptions;

  # Fix reflex-platform#161
  #TODO: Upstream this into nixpkgs
  simple-sendfile = addBuildDepends super.simple-sendfile (with self; [
    conduit
    conduit-extra
    resourcet
  ]);

  mkDerivation = drv: super.mkDerivation (drv // {
    doHaddock = false;
    dontStrip = true;
    enableSharedExecutables = false;
    configureFlags = (drv.configureFlags or []) ++ [
      "--ghc-option=-fPIC"
      "--ghc-option=-optc-fPIC"
      "--ghc-option=-optc-shared"
      "--ghc-option=-optl-shared"
    ];
  });
}
