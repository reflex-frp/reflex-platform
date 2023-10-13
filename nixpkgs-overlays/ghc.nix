{ lib }:

self: super: {
  cabal2nix-unwrapped = self.haskell.packages.ghc8107.cabal2nix;
  cabal-install = self.haskell.packages.ghc8107.cabal-install;
  # Apply custom patches to Haskell compilers
  haskell = super.haskell // {
    compiler = super.haskell.compiler // {
      ghc8107 = super.haskell.compiler.ghc8107.overrideAttrs (drv: {
        patches = (drv.patches or []) ++ lib.optionals (self.stdenv.targetPlatform.isiOS || self.stdenv.targetPlatform.isAarch64 && self.stdenv.targetPlatform.isDarwin) [
          (super.fetchurl {
            url = "https://raw.githubusercontent.com/reflex-frp/reflex-platform/mars/modules/patches/aarch64-darwin/fix_dead_strip.patch";
            sha256 = "sha256-1VcftxpaMmLzMnB8X4M6Xg9o+OmgpaNOeF7Yrn1x0EI=";
          })
        ];
      });
    };
    packages = super.haskell.packages // {
      ghc8107 = super.haskell.packages.ghc8107.override {
        buildHaskellPackages = self.buildPackages.haskell.packages.ghc8107;
        ghc = self.buildPackages.haskell.compiler.ghc8107;
      };
    };
  };
}
