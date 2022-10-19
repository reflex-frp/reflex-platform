{ lib, haskellLib, fetchFromGitHub, fetchpatch, versionWildcard, thunkSet }:

with lib;
with haskellLib;

self: super: {
  _dep = super._dep or { } // thunkSet ./dep // { };

  attoparsec = dontCheck (self.callCabal2nix "attoparsec" self._dep.attoparsec { });
  buffer-builder = appendPatch super.buffer-builder ./buffer-builder-text-jsstring.patch;
  conduit-extra = (appendPatch super.conduit-extra ./conduit-extra-text-jsstring.patch);
  double-conversion = appendPatch super.double-conversion ./double-conversion-Add-text-jsstring.patch;
}
