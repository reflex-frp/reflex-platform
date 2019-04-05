{ lib }:

self: super: {
  haskell = super.haskell // {
    compiler = super.haskell.compiler // lib.mapAttrs (n: v: v.overrideAttrs (drv: {
      patches = (drv.patches or []) ++ lib.optionals self.stdenv.targetPlatform.useAndroidPrebuilt [
        ./8.4.y/android-patches/force-relocation.patch
      ];
    })) { inherit (super.haskell.compiler) ghc844 ghcHEAD ghcSplices; };
  };
}
