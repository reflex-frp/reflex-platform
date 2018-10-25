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

  entropy = haskellLib.overrideCabal super.entropy (drv: {
    src = fetchFromGitHub {
      owner = "TomMD";
      repo = "entropy";
      rev = "c682fcd6cc2bcaa5cab9f6e59a93faf8c9938221";
      sha256 = "162pazw91zp3kzvivxdn3n8ffsdxi9bfwm24vwkp96sgds59xs31";
    };
  });

  vector-algorithms = haskellLib.overrideCabal super.entropy (drv: {
    src = fetchFromGitHub {
      owner = "matthewbauer";
      repo = "vector-algorithms";
      rev = "7acf1e142f352d728b0b3a050da825d472fe6471";
      sha256 = "0b5pnjfwgjm2jk2hhld4y217qa3j1vvzv68h8daxnwrp07q1fyka";
    };
  });

}
