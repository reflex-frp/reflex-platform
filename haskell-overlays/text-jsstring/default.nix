{ haskellLib, fetchFromGitHub }:

self: super: {
  text = self.callCabal2nix "text" (fetchFromGitHub {
    owner = "luigy";
    repo = "text";
    rev = "6cc95ebb07c07001666d84ace5c13caefaaa0cad";
    sha256 = "1zplzy9mfpwjrk5l22gmla1vmk7wmwmgmjfk64b57ysn7madlv19";
  }) {};
  ghcjs-base = haskellLib.overrideCabal super.ghcjs-base (drv: {
    src = fetchFromGitHub {
      owner = "luigy";
      repo = "ghcjs-base";
      rev = "e287c5752064a2d3b2c4776a1520e4b0189881b0";
      sha256 = "01k7wj60gmmf9larjm3gqbsyxwb5xhqr4dyz4xswy78ql845qljd";
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
  buffer-builder = haskellLib.overrideCabal super.buffer-builder (drv: {
    doCheck = false;
    src = fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "buffer-builder";
      rev = "59c730e0dec7ff0efd8068250f4bca9cb74c471d";
      sha256 = "18dd2ydva3hnsfyrzmi3y3r41g2l4r0kfijaan85y6rc507k6x5c";
    };
  });
  hashable = haskellLib.addBuildDepend (self.callCabal2nix "hashable" (fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "hashable";
    rev = "1008a580f2dd3ebd4931e7d8cb36d1347a1e9dc6";
    sha256 = "1zdd7qjv9k6332h4c6frjjfavknzzffw4ayv8q4f2zh9w774hzli";
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
  aeson = haskellLib.appendPatch super.aeson ./aeson.patch;
}
