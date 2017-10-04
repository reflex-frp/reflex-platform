{ haskellLib, fetchFromGitHub }:

self: super: {
  text = self.callCabal2nix "text" (fetchFromGitHub {
    owner = "luigy";
    repo = "text";
    rev = "6cc95ebb07c07001666d84ace5c13caefaaa0cad";
    sha256 = "1zplzy9mfpwjrk5l22gmla1vmk7wmwmgmjfk64b57ysn7madlv19";
  }) {};
  jsaddle = haskellLib.overrideCabal super.jsaddle (drv: {
    patches = (drv.patches or []) ++ [
      ./jsaddle-text-jsstring.patch
    ];
    buildDepends = (drv.buildDepends or []) ++ [
      self.ghcjs-json
    ];
  });
  ghcjs-json = self.callCabal2nix "ghcjs-json" (fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "ghcjs-json";
    rev = "3a6e1e949aced800d32e0683a107f5387295f3a6";
    sha256 = "1pjsvyvy6ac3358db19iwgbmsmm0si2hzh2ja1hclq43q6d80yij";
  }) {};
  ghcjs-base = haskellLib.overrideCabal super.ghcjs-base (drv: {
    src = fetchFromGitHub {
      owner = "luigy";
      repo = "ghcjs-base";
      rev = "8569f5d541aa846f2130ff789d19bcd55ea41d2a";
      sha256 = "1b1fyqgn7jxh4rawgxidacafg6jwfdfcidyh93z6a6lhmm5qaq3n";
    };
    libraryHaskellDepends = with self; [
      base bytestring containers deepseq dlist ghc-prim
      ghcjs-prim integer-gmp primitive time
      transformers vector
    ];
  });
  attoparsec = haskellLib.overrideCabal super.attoparsec (drv: {
    src = fetchFromGitHub {
      owner = "luigy";
      repo = "attoparsec";
      rev = "e766a754811042f061b6b4498137d2ad28e207a8";
      sha256 = "106fn187hw9z3bidbkp7r4wafmhk7g2iv2k0hybirv63f8727x3x";
    };
  });
  hashable = haskellLib.addBuildDepend (self.callCabal2nix "hashable" (fetchFromGitHub {
    owner = "luigy";
    repo = "hashable";
    rev = "97a6fc77b028b4b3a7310a5c2897b8611e518870";
    sha256 = "1rl55p5y0mm8a7hxlfzhhgnnciw2h63ilxdaag3h7ypdx4bfd6rs";
  }) {}) self.text;
  conduit-extra = haskellLib.overrideCabal super.conduit-extra (drv: {
    src = "${fetchFromGitHub {
      owner = "luigy";
      repo = "conduit";
      rev = "aeb20e4eb7f7bfc07ec401c82821cbb04018b571";
      sha256 = "10kz2m2yxyhk46xdglj7wdn5ba2swqzhyznxasj0jvnjcnv3jriw";
    }}/conduit-extra";
  });
  double-conversion = haskellLib.overrideCabal super.double-conversion (drv: {
    src = fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "double-conversion";
      rev = "0f9ddde468687d25fa6c4c9accb02a034bc2f9c3";
      sha256 = "0sjljf1sbwalw1zycpjf6bqhljag9i1k77b18b0fd1pzrc29wnks";
    };
  });
  say = haskellLib.overrideCabal super.say (drv: {
    patches = (drv.patches or []) ++ [
      ./say-text-jsstring.patch
    ];
    buildDepends = (drv.buildDepends or []) ++ [
      self.ghcjs-base
    ];
  });
}
