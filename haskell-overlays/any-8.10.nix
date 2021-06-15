{ haskellLib }:
self: super: {
  stylish-haskell = haskellLib.dontCheck super.stylish-haskell;
}
