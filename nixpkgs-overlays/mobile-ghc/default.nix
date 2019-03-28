{ lib }:

self: super: {
  haskell = super.haskell // {
    compiler = super.haskell.compiler // lib.mapAttrs (n: v: v.overrideAttrs (drv: {
      patches = (drv.patches or []) ++ lib.optionals self.stdenv.targetPlatform.useAndroidPrebuilt [
        ./8.4.y/android-patches/force-relocation.patch
        ./8.4.y/android-patches/force-armv7.patch
      ];
      # Since force-armv7.patch applies to aclocal.m4, we need to regenerate configure.
      nativeBuildInputs = (drv.nativeBuildInputs or []) ++ [self.autoreconfHook];
    })) { inherit (super.haskell.compiler) ghc843 ghcHEAD ghcSplices; };
  };
}
