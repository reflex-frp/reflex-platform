{ lib }:
self: super: {
  haskell = super.haskell // {
    compiler = super.haskell.compiler // lib.mapAttrs (n: v: v.overrideAttrs (drv: {
      patches = (drv.patches or []) ++ lib.optionals self.stdenv.targetPlatform.useAndroidPrebuilt [
        ./8.6.y/android-patches/force-relocation.patch
      ];
    })) { inherit (super.haskell.compiler) ghc865 ghcSplices-8_6 ghc8107 ghc8107Splices; };
  };
}
