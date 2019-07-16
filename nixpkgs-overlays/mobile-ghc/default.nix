{ lib }:

self: super: {
  haskell = super.haskell // {
    compiler = super.haskell.compiler // lib.mapAttrs (n: v: v.overrideAttrs (drv: {
      patches = (drv.patches or []) ++ lib.optionals self.stdenv.targetPlatform.useAndroidPrebuilt [
        ./8.6.y/android-patches/force-relocation.patch
      ];
    })) { inherit (super.haskell.compiler) ghc865 ghcSplices; };
  };
  libffi = if self.stdenv.targetPlatform.useAndroidPrebuilt
    then
      super.libffi.overrideAttrs (drv: {
        name = "libffi-3.3-rc0";
        patches = [];
        src = builtins.fetchurl {
          url = "https://github.com/libffi/libffi/releases/download/v3.3-rc0/libffi-3.3-rc0.tar.gz";
          sha256 = "1kqfyar1xaxrylrw7j1nclqvcgr6a2cisazaamw1a18wpym6fga0";
        };
        postFixup = "";
      })
    else
      super.libffi;
}
