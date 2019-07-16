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
      # Use libffi-3.3-rc0 because the regular libffi is not compatible with android-ndk.
      # Google has its own forked libffi (https://android.googlesource.com/platform/external/libffi/)
      # but upstream also has a release candidate that fixes the same issues. We're using upstream here:
      super.libffi.overrideAttrs (drv: {
        name = "libffi-3.3-rc0";
        patches = [];
        src = builtins.fetchurl {
          url = "https://github.com/libffi/libffi/releases/download/v3.3-rc0/libffi-3.3-rc0.tar.gz";
          sha256 = "1kqfyar1xaxrylrw7j1nclqvcgr6a2cisazaamw1a18wpym6fga0";
        };
        # The new version of libffi doesn't require the same post-fixup hacks that 3.2.1 required. Also,
        # the postFixup script in nixpkgs-channel 19.03 hardcoded some paths to include "3.2.1" specifically,
        # which is no longer valid.
        postFixup = "";
      })
    else
      super.libffi;
}
