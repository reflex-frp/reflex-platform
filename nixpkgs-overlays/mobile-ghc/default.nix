{ lib, pkgs, nixpkgsCross  }:
let
  versionWildcard = versionList: let
    versionListInc = lib.init versionList ++ [ (lib.last versionList + 1) ];
    bottom = lib.concatStringsSep "." (map toString versionList);
    top = lib.concatStringsSep "." (map toString versionListInc);
  in version: lib.versionOlder version top && lib.versionAtLeast version bottom;
in self: super: {
  haskell = super.haskell // {
    compiler = super.haskell.compiler //  lib.mapAttrs (n: v: (v.override {
      enableDocs = false;
      # ghcFlavour = "quick-cross-ncg";
      libiconv =
       with self.stdenv.targetPlatform;  if useAndroidPrebuilt then
         (with  nixpkgsCross.android; if is32bit then aarch32 else aarch64).libiconv.overrideAttrs (drv:
            {
              configureFlags = ["--disable-shared" "--enable-static"];
            }
          )
	  else if isiOS then
	   (with  nixpkgsCross.ios; if is32bit then aarch32 else aarch64).libiconv.overrideAttrs (drv:
            {
              configureFlags = ["--disable-shared" "--enable-static"];
            }
          )
	  else if isMacOS then pkgs.libiconv
	  else null;
    }).overrideAttrs (drv: {
      patches =
        let isAndroid = self.stdenv.targetPlatform.useAndroidPrebuilt;
        in
          (drv.patches or []) ++
          lib.optionals isAndroid [
            ./8.6.y/android-patches/force-relocation.patch 
          ];

      nativeBuildInputs =
        let bootPkgs = drv.passthru.bootPkgs; in
        with pkgs; [
          perl autoconf269 automake m4 python3
          bootPkgs.ghc
          bootPkgs.alex bootPkgs.happy_1_19_12 bootPkgs.hscolour
        ];
    })){ inherit (super.haskell.compiler) ghc8107 ghcSplices-8_10; };
  };
}
