{ haskellLib }:

self: super: {
  mkDerivation = drv: super.mkDerivation (drv // {
    enableSplitObjs = false; # Split objects with template haskell doesn't work on ghc 7.8
  });
  MemoTrie = haskellLib.addBuildDepend super.MemoTrie self.void;
  generic-deriving = haskellLib.dontHaddock super.generic-deriving;
  bifunctors = haskellLib.dontHaddock super.bifunctors;
  cereal = haskellLib.dontCheck super.cereal; # cereal's test suite requires a newer version of bytestring than this haskell environment provides
}
