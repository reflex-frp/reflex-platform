{ lib }:

self: super: {
  haskell = super.haskell // {
    compiler = super.haskell.compiler // {
      ghc822 = super.haskell.compiler.ghc822.overrideAttrs (drv: {
        patches = (drv.patches or []) ++ lib.optionals self.stdenv.targetPlatform.useAndroidPrebuilt [
          ./8.2.y/android-patches/add-llvm-target-data-layout.patch
          ./8.2.y/android-patches/unix-posix_vdisable.patch
          ./8.2.y/android-patches/force_CC_SUPPORTS_TLS_equal_zero.patch
          ./8.2.y/android-patches/undefine_MYTASK_USE_TLV_for_CC_SUPPORTS_TLS_zero.patch
          ./8.2.y/android-patches/force-relocation-equal-pic.patch
          ./8.2.y/android-patches/rts_android_log_write.patch
        ] ++ lib.optionals self.stdenv.targetPlatform.isiOS [
          ./8.2.y/fixed-ios.patch
          #(self.buildPackages.fetchpatch {
          #  url = "https://patch-diff.githubusercontent.com/raw/obsidiansystems/ghc/pull/1.patch";
          #  sha256 = "0h34fd55qng3xrnm6x6jy56f7rbmng55afy56r84lvrlk1y8gjwf";
          #  excludes = [ "testsuite" ];
          #})
        ];
      });
    } // lib.mapAttrs (n: v: v.overrideAttrs (drv: {
        patches = (drv.patches or []) ++ lib.optionals self.stdenv.targetPlatform.useAndroidPrebuilt [
          ./8.4.y/android-patches/force-relocation.patch
        ];
    })) { inherit (super.haskell.compiler) ghc843 ghcHEAD ghcSplices; };
  };
}
