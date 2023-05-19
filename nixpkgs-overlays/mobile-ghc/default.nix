{ lib }:
let
  versionWildcard = versionList: let
    versionListInc = lib.init versionList ++ [ (lib.last versionList + 1) ];
    bottom = lib.concatStringsSep "." (map toString versionList);
    top = lib.concatStringsSep "." (map toString versionListInc);
  in version: lib.versionOlder version top && lib.versionAtLeast version bottom;
in self: super: {
  haskell = super.haskell // {
    compiler = super.haskell.compiler // lib.mapAttrs (n: v: v.overrideAttrs (drv: {
      patches = let
        isAndroid = self.stdenv.targetPlatform.useAndroidPrebuilt;
        isGhc86x = versionWildcard [ 8 6 ] v.version;
      in
        (drv.patches or []) ++
        lib.optionals isAndroid [
          ./8.6.y/android-patches/force-relocation.patch
        ] ++
        lib.optionals (isAndroid && isGhc86x) [
          ./8.6.y/android-patches/strict-align.patch
        ];
    })) { inherit (super.haskell.compiler) ghc8107 ghcSplices-8_10; };
  };
}

