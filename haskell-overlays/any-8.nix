{ lib, haskellLib, getGhcVersion }:
with haskellLib;
self: super: lib.optionalAttrs (lib.versionOlder (getGhcVersion super.ghc) "8.6.0") {
}
