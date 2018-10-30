{ haskellLib }:
with haskellLib;
self: super: {
  # Setup: Unknown build tool hspec-discover
  megaparsec = haskellLib.dontCheck super.megaparsec;
  # Setup: Unknown build tool hspec-discover
  modern-uri = haskellLib.dontCheck super.modern-uri;
  # Version compatible with ghc-mod 0.5.8.0
  cabal-helper = haskellLib.doJailbreak (self.callHackage "cabal-helper" "0.7.3.0" {});
  # missing semigroups pkg
  ListLike = haskellLib.addBuildDepend super.ListLike self.semigroups;
  #
  ghc-mod = haskellLib.doJailbreak super.ghc-mod;
}
