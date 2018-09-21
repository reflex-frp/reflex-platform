{ haskellLib, fetchFromGitHub }:
with haskellLib;
self: super: {
  gtk2hs-buildtools = self.callHackage "gtk2hs-buildtools" "0.13.3.0" {};
  stylish-haskell = self.callHackage "stylish-haskell" "0.9.0.2" {};
}
