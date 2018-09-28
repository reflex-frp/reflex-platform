{ lib, haskellLib, getGhcVersion, pkgs }:

self: super: lib.optionalAttrs (lib.versionOlder (getGhcVersion super.ghc) "8.4.0") {
  # TODO document why
  concurrent-output = haskellLib.doJailbreak super.concurrent-output;
  # Newer versions cause some sort of issues with multiple `(<>)` definitions.
  # Seems to be that semigroup-monoid stuff is being CPP'd incorrectly.
  base-compat= self.callHackage "base-compat" "0.9.3" {};
  # `configure` cannot be generated on the fly from `configure.ac` with older
  # Cabal.
  old-time = haskellLib.addBuildTool super.old-time pkgs.autoreconfHook;
}
