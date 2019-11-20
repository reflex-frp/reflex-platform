{ lib }:

self: super: {
  # Apply custom patches to Haskell compilers
  haskell = super.haskell // {
    compiler = super.haskell.compiler // lib.mapAttrs (n: v: v.overrideAttrs (drv: {
      patches = (drv.patches or []) ++ [
        # Fix a segfault in base reported in https://gitlab.haskell.org/ghc/ghc/issues/16893
        ./bug-16893-workaround.patch
      ];
    })) { inherit (super.haskell.compiler) ghc843; };
  };
}
