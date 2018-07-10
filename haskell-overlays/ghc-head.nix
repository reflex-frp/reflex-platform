{ haskellLib, fetchFromGitHub }:

self: super: {
  th-expand-syns = haskellLib.doJailbreak super.th-expand-syns;
  ChasingBottoms = haskellLib.doJailbreak super.ChasingBottoms;
  base-orphans = haskellLib.dontCheck super.base-orphans;
  bifunctors = haskellLib.dontCheck super.bifunctors;
  HTTP = haskellLib.doJailbreak super.HTTP;
  newtype-generics = haskellLib.doJailbreak super.newtype-generics;

  # extra = haskellLib.replaceSrc super.extra (fetchFromGitHub {
  #   owner = "ndmitchell";
  #   repo = "extra";
  #   rev = "22b0e6aa6077b2d969e8b8ac613f5a3455d9e88d";
  #   sha256 = "0milbw2azkj22rqacrnd0x4wh65qfrl3nhbmwfxzmdrsc2la3bkh";
  # }) "1.5.2";

  integer-logarithms = haskellLib.doJailbreak super.integer-logarithms;
  tagged = self.callHackage "tagged" "0.8.6" {};
  vector = haskellLib.doJailbreak super.vector;
  th-abstraction = self.callHackage "th-abstraction" "0.2.8.0" {};
  exceptions = self.callHackage "exceptions" "0.10.0" {};

  stm = haskellLib.overrideCabal super.stm (drv: {
    src = fetchFromGitHub {
      owner = "haskell";
      repo = "stm";
      rev = "c107caefc08606f231dd6e8b9e0f1295e44bd846";
      sha256 = "07m4bkizsbv2gclrydja3dkjjgyhaqnzgh9zfsp9dm5y7hz8xlj9";
    };
  });
}
