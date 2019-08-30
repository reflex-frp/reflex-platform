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
  async = haskellLib.doJailbreak super.async;
  th-abstraction = haskellLib.doJailbreak super.th-abstraction;
  th-lift = haskellLib.doJailbreak super.th-lift;
  integer-logarithms = haskellLib.doJailbreak super.integer-logarithms;
  vault = haskellLib.doJailbreak super.vault;
  unliftio-core = haskellLib.doJailbreak super.unliftio-core;
  unix-compat = haskellLib.doJailbreak super.unix-compat;
  bsb-http-chunked = haskellLib.doJailbreak super.bsb-http-chunked;
  parsec = haskellLib.doJailbreak super.parsec;
  lens = haskellLib.doJailbreak super.lens;
  jsaddle = haskellLib.doJailbreak super.jsaddle;
  exceptions = haskellLib.doJailbreak super.exceptions;
  keycode = haskellLib.doJailbreak super.keycode;

  aeson = super.aeson_1_4_0_0;

  tagged = self.callHackage "tagged" "0.8.6" {};
  contravariant = self.callHackage "contravariant" "1.5" {};

  stm = haskellLib.overrideCabal super.stm (drv: {
    src = fetchFromGitHub {
      owner = "haskell";
      repo = "stm";
      rev = "c107caefc08606f231dd6e8b9e0f1295e44bd846";
      sha256 = "07m4bkizsbv2gclrydja3dkjjgyhaqnzgh9zfsp9dm5y7hz8xlj9";
    };
  });

  entropy = self.callHackage "entropy" "0.4.1.4" {};

}
