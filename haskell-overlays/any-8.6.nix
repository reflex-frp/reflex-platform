{ pkgs, haskellLib, fetchFromGitHub }:
with haskellLib;
self: super: {
  cabal-macosx = dontCheck super.cabal-macosx;
  enclosed-exceptions = dontCheck super.enclosed-exceptions; # see https://github.com/jcristovao/enclosed-exceptions/issues/12
  haddock-library-ghcjs = dontCheck super.haddock-library-ghcjs;
  haddock-api-ghcjs = dontCheck super.haddock-api-ghcjs;
  algebraic-graphs = doJailbreak super.algebraic-graphs;

  # Broken in master
  # PR is https://github.com/vincenthz/hit/pull/37
  # hit = dontCheck (self.callCabal2nix "hit" (fetchFromGitHub {
  #   owner = "vincenthz";
  #   repo = "hit";
  #   rev = "e93b01a295d5b4ca51b32b928b37ae040366e317";
  #   sha256 = "1vfxqc3kffls11dzxq0gk62ky8rjm455cnh0nv31x43g3pmhh7sp";
  # }) {});

  # doctests: doctests: could not execute: markdown-unlit
  # Test suite doctests: FAIL
  # rank2classes = dontCheck super.rank2classes;
  # entropy = self.callHackage "entropy" "0.4.1.4" {};
  # cryptohash-sha256 = doJailbreak super.cryptohash-sha256;
  # czipwith = doJailbreak super.czipwith;
  # haddock-library = doJailbreak (self.callHackage "haddock-library" "1.7.0" {});
  # basement = self.callHackage "basement" "0.0.10" {};
  # keycode = doJailbreak (self.callCabal2nix "hit" (fetchFromGitHub {
  #   owner = "RyanGlScott";
  #   repo = "keycode";
  #   rev = "beecb745750de7b0b470ae5af9f2fe506f54dd31";
  #   sha256 = "03zm21f134cpg13fhnm541hawz649ynwmcwwmaz358gdnd2fypgv";
  # }) {});
  # polyparse = self.callHackage "polyparse" "1.12.1" {};
}
