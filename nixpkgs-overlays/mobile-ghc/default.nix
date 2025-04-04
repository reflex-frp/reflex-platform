{ lib }:
self: super: {
  haskell = super.haskell // {
    compiler = super.haskell.compiler // lib.mapAttrs (n: v: v.overrideAttrs (drv: {
      patches = let
        isAndroid = self.stdenv.targetPlatform.useAndroidPrebuilt;
      in
        (drv.patches or []) ++
        lib.optionals isAndroid [
          ./android-patches/force-relocation.patch
        ];
    })) { inherit (super.haskell.compiler) ghc8107 ghcSplices-8_10; };
  };
}
