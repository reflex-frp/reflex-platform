{ lib, haskellLib, fetchFromGitHub, hackGet }:

with lib;
with haskellLib;

self: super: {
  text = self.callCabal2nix "text" (fetchFromGitHub {
    owner = "luigy";
    repo = "text";
    rev = "3f2de34fae6da19cdd92f1bc6b722d4537b92010";
    sha256 = "15rahd7dip5gyckh06rm5q2q230wk4snbfnf3rkcr7qr37h2qzgd";
  }) {};
  jsaddle = overrideCabal super.jsaddle (drv: {
    buildDepends = (drv.buildDepends or []) ++ [
      self.ghcjs-base
      self.ghcjs-prim
    ];
  });
  ghcjs-base = overrideCabal super.ghcjs-base (drv: {
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
  attoparsec = overrideCabal super.attoparsec (drv: {
    src = fetchFromGitHub {
      owner = "luigy";
      repo = "attoparsec";
      rev = "e766a754811042f061b6b4498137d2ad28e207a8";
      sha256 = "106fn187hw9z3bidbkp7r4wafmhk7g2iv2k0hybirv63f8727x3x";
    };
  });
  buffer-builder = overrideCabal super.buffer-builder (drv: {
    doCheck = false;
    src = fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "buffer-builder";
      rev = "59c730e0dec7ff0efd8068250f4bca9cb74c471d";
      sha256 = "18dd2ydva3hnsfyrzmi3y3r41g2l4r0kfijaan85y6rc507k6x5c";
    };
  });
  hashable = overrideCabal super.hashable (drv: {
    revision = null;
    editedCabalFile = null;
    jailbreak = true;
    doCheck = false;
    libraryHaskellDepends = (drv.libraryHaskellDepends or []) ++ [
      self.text
    ];
    patches = (drv.patches or []) ++ [
      ./hashable.patch
    ];
  });
  conduit-extra = overrideCabal super.conduit-extra (drv: {
    src = "${fetchFromGitHub {
      owner = "luigy";
      repo = "conduit";
      rev = "aeb20e4eb7f7bfc07ec401c82821cbb04018b571";
      sha256 = "10kz2m2yxyhk46xdglj7wdn5ba2swqzhyznxasj0jvnjcnv3jriw";
    }}/conduit-extra";
  });
  double-conversion = overrideCabal super.double-conversion (drv: {
    src = fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "double-conversion";
      rev = "0f9ddde468687d25fa6c4c9accb02a034bc2f9c3";
      sha256 = "0sjljf1sbwalw1zycpjf6bqhljag9i1k77b18b0fd1pzrc29wnks";
    };
  });
  say = overrideCabal super.say (drv: {
    patches = (drv.patches or []) ++ [
      ./say.patch
    ];
    buildDepends = (drv.buildDepends or []) ++ [
      self.ghcjs-base
    ];
  });
  vector = overrideCabal super.vector (drv: {
    buildDepends = filter (p: p.pname != "semigroups") drv.buildDepends;
  });

  #TODO: Fix this failure
  th-lift-instances = dontCheck super.th-lift-instances;

  aeson = appendPatch super.aeson ./aeson.patch;
}
