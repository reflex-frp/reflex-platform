{ lib }:

self: super: {
  # Apply custom patches to Haskell compilers
  haskell = super.haskell // {
    compiler = super.haskell.compiler // lib.mapAttrs (n: v: v.overrideAttrs (drv: {
      patches = (drv.patches or []) ++ [
        # Fix a segfault in base reported in https://gitlab.haskell.org/ghc/ghc/issues/16893
        (self.fetchpatch {
          url = "https://gitlab.haskell.org/ghc/ghc/commit/220ab887a4f009d9da8b2336d228d9f8eaf4aacc.patch";
          sha256 = "1qjvjxbn8xczmxm8pc9vxnqwn2nx0xyfm0ifkpa3sqswy3a09x9j";
        })
      ];
    })) { inherit (super.haskell.compiler) ghc865; };
  };
}
