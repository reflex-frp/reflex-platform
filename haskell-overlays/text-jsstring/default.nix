{ lib, haskellLib, fetchFromGitHub, hackGet, fetchpatch }:

with lib;
with haskellLib;

self: super: {
  text = self.callCabal2nix "text" (fetchFromGitHub {
     owner = "obsidiansystems";
     repo = "text";
     rev = "9b5646358d4d562db610de5fa51a1853b6161e08";
     sha256 = "0hybnzmp38jvz9289si6x6ciayjiay84sw7mgba2hwrb5x54zzr4";
  }) {};
  jsaddle = overrideCabal super.jsaddle (drv: {
    buildDepends = (drv.buildDepends or []) ++ [
      self.ghcjs-base
      self.ghcjs-prim
    ];
  });
  ghcjs-base = overrideCabal super.ghcjs-base (drv: {
    patches = (drv.patches or []) ++ [
      ./ghcjs-base-text-jsstring.patch
      (fetchpatch {
        url = "https://patch-diff.githubusercontent.com/raw/ghcjs/ghcjs-base/pull/106.patch";
        sha256 = "0hdagclc6hkd0kc3cjcy61xz99qvv55yrgc4szgmknkl5lq8y3yd";
      })
    ];
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
