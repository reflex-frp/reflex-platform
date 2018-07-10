{ haskellLib, fetchFromGitHub }:

self: super: {
  th-expand-syns = haskellLib.doJailbreak super.th-expand-syns;
  ChasingBottoms = haskellLib.doJailbreak super.ChasingBottoms;
  base-orphans = haskellLib.dontCheck super.base-orphans;
  bifunctors = haskellLib.doJailbreak (haskellLib.dontCheck super.bifunctors);
  HTTP = haskellLib.doJailbreak super.HTTP;
  newtype-generics = haskellLib.doJailbreak super.newtype-generics;
  split = haskellLib.doJailbreak super.split;
  StateVar = haskellLib.doJailbreak super.StateVar;
  ref-tf = haskellLib.doJailbreak super.ref-tf;
  parallel = haskellLib.doJailbreak super.parallel;
  cabal-doctest = haskellLib.doJailbreak super.cabal-doctest;
  vector = haskellLib.doJailbreak super.vector;
  pointed = haskellLib.doJailbreak super.pointed;
  exception-transformers = haskellLib.doJailbreak super.exception-transformers;

  aeson = super.aeson_1_4_0_0;

  integer-logarithms = haskellLib.doJailbreak super.integer-logarithms;
  tagged = self.callHackage "tagged" "0.8.6" {};
  th-abstraction = self.callHackage "th-abstraction" "0.2.8.0" {};
  exceptions = self.callHackage "exceptions" "0.10.0" {};
  contravariant = self.callHackage "contravariant" "1.5" {};

  stm = haskellLib.overrideCabal super.stm (drv: {
    src = fetchFromGitHub {
      owner = "haskell";
      repo = "stm";
      rev = "c107caefc08606f231dd6e8b9e0f1295e44bd846";
      sha256 = "07m4bkizsbv2gclrydja3dkjjgyhaqnzgh9zfsp9dm5y7hz8xlj9";
    };
  });

}
