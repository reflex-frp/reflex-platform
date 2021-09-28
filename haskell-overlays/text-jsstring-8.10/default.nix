{ lib, haskellLib, fetchFromGitHub, fetchpatch, versionWildcard, thunkSet }:

with lib;
with haskellLib;

self: super: {
  _dep = super._dep or {} // thunkSet ./dep // {
    ghcjsBaseTextJSStringSrc = super.ghcjs-base.src.overrideAttrs (drv: {
      outputHash = "1pdxlb67f94bl2b6k0m4flpjbf07g7fgqbyjnki4y57a5r0iympd";
      postFetch = (drv.postFetch or "") + ''
        ( cd $out
          patch -p1 < ${./ghcjs-base-text-jsstring.patch}
        )
      '';
    });
  };

  jsaddle = overrideCabal super.jsaddle (drv: {
    buildDepends = (drv.buildDepends or []) ++ [
      self.ghcjs-base
      self.ghcjs-prim
    ];
  });

  aeson = dontCheck (self.callCabal2nix "aeson" self._dep.aeson {});
  attoparsec = dontCheck (self.callCabal2nix "attoparsec" self._dep.attoparsec {});
  buffer-builder = appendPatch super.buffer-builder ./buffer-builder-text-jsstring.patch;
  conduit-extra =  (appendPatch super.conduit-extra ./conduit-extra-text-jsstring.patch);
  double-conversion = appendPatch super.double-conversion ./double-conversion-Add-text-jsstring.patch;
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
  say = overrideCabal super.say (drv: {
    patches = (drv.patches or []) ++ [
      ./say.patch
    ];
    buildDepends = (drv.buildDepends or []) ++ [
      self.ghcjs-base
    ];
  });
}
