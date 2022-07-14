{ lib, haskellLib, fetchFromGitHub, fetchpatch, versionWildcard, thunkSet }:

with lib;
with haskellLib;

self: super: {
  _dep = super._dep or {} // thunkSet ./dep // {
  };

  jsaddle = overrideCabal super.jsaddle (drv: {
    buildDepends = (drv.buildDepends or []) ++ [
      self.ghcjs-base
      self.ghcjs-prim
    ];
  });

  aeson = dontCheck (self.callCabal2nix "aeson" self._dep.aeson {});
  attoparsec = dontCheck (self.callCabal2nix "attoparsec" self._dep.attoparsec {});
  hashable = self.callHackage "hashable" "1.4.0.2" {};
  # hashable = overrideCabal super.hashable (drv: {
  #   revision = null;
  #   editedCabalFile = null;
  #   jailbreak = true;
  #   doCheck = false;
  #   libraryHaskellDepends = (drv.libraryHaskellDepends or []) ++ [
  #     self.text
  #   ];
  #   patches = (drv.patches or []) ++ [
  #     ./hashable.patch
  #   ];
  # });
  say = overrideCabal super.say (drv: {
    patches = (drv.patches or []) ++ [
      ./say.patch
    ];
    buildDepends = (drv.buildDepends or []) ++ [
      self.ghcjs-base
    ];
  });
}
