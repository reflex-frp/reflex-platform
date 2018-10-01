{ haskellLib, stage2Script }:

self: super: {
  ########################################################################
  # Synchronize packages with ghcjs
  ########################################################################
  # aeson-0.11.2.0's tests can't build with QuickCheck >= 2.9, because
  # some instances have been added to QuickCheck which overlap with ones
  # defined by aeson.  This can probably be removed once ghcjs-boot has
  # updated to aeson >= 0.11.2.1.
  aeson = let version = (import stage2Script { ghcjsBoot = null; } { inherit (self) callPackage; }).aeson.version;
    in haskellLib.dontCheck (self.callPackage (self.hackage2nix "aeson" version) {});
}
