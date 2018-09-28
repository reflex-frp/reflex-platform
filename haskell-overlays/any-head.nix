{ haskellLib, fetchFromGitHub }:

self: super: {
  th-expand-syns = haskellLib.doJailbreak super.th-expand-syns;
  ChasingBottoms = haskellLib.doJailbreak super.ChasingBottoms;
  base-orphans = haskellLib.dontCheck super.base-orphans;
  bifunctors = haskellLib.dontCheck super.bifunctors;
  HTTP = haskellLib.doJailbreak super.HTTP;
  newtype-generics = haskellLib.doJailbreak super.newtype-generics;
  extra = haskellLib.replaceSrc super.extra (fetchFromGitHub {
    owner = "ndmitchell";
    repo = "extra";
    rev = "22b0e6aa6077b2d969e8b8ac613f5a3455d9e88d";
    sha256 = "0milbw2azkj22rqacrnd0x4wh65qfrl3nhbmwfxzmdrsc2la3bkh";
  }) "1.5.2";
}
