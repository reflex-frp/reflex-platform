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
  _dep = super._dep or { } // thunkSet ./dep;

  android-activity = haskellLib.overrideCabal
    (self.callCabal2nix "android-activity" (self._dep.android-activity + "/") {
      log = nixpkgs.androidndkPkgs_24.libraries;
    })
    (drv:
      let
        jdk-fixed = (nixpkgs.buildPackages.jdk17.override {
          headless = true;
          enableGnome2 = false;
          enableJavaFX = false;
          openjdk17-bootstrap = nixpkgs.buildPackages.openjdk17-bootstrap.override {
            gtkSupport = false;
          };
        });
      in
      {
        librarySystemDepends = (drv.librarySystemDepends or [ ]) ++ [ jdk-fixed ];
        enableSharedExecutables = true;
        enableSharedLibraries = true;
        enableStaticLibraries = false;
        buildTools = (drv.buildTools or [ ]) ++ [ ];
        configureFlags = (drv.configureFlags or [ ]) ++ [
          "--enable-shared"
          #"-v3"
        ];
      });

  syb = haskellLib.overrideCabal super.syb (drv: { jailbreak = true; });
  cabal-doctest = null;

  # Break version bounds on base for GHC HEAD.
  lifted-async = haskellLib.doJailbreak super.lifted-async;
  safe-exceptions = haskellLib.doJailbreak super.safe-exceptions;


  blaze-textual = haskellLib.enableCabalFlag super.blaze-textual "integer-simple";
  cryptonite = haskellLib.disableCabalFlag super.cryptonite "integer-gmp";

  OneTuple = haskellLib.overrideCabal super.OneTuple (drv: {
    libraryHaskellDepends = (drv.libraryHaskellDepends or [ ]) ++ [
      self.hashable
    ];
  });
  hashable = enableFPic super.hashable;
  primitive = enableFPic super.primitive;

  mkDerivation = drv: super.mkDerivation (drv // {
    doHaddock = false;
    dontStrip = true;
    enableSharedExecutables = false;
  });

  # HACK(Dylan Green):
  # Temporary fix for RPATH troubles regarding attoparsec
  attoparsec = haskellLib.overrideCabal super.attoparsec (drv: {
    preFixup = ''
      rm -rf "$(pwd)"
      mkdir "$(pwd)"
    '';
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

  # Tests take a long time
  vector = haskellLib.dontCheck super.vector;
}
