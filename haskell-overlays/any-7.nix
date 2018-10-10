{ haskellLib }:

self: super: {
  cereal = haskellLib.dontCheck super.cereal; # cereal's test suite requires a newer version of bytestring than this haskell environment provides
}
