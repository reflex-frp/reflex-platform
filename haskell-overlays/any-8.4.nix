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
  hit = dontCheck (self.callCabal2nix "hit" (fetchFromGitHub {
    owner = "vincenthz";
    repo = "hit";
    rev = "e93b01a295d5b4ca51b32b928b37ae040366e317";
    sha256 = "1vfxqc3kffls11dzxq0gk62ky8rjm455cnh0nv31x43g3pmhh7sp";
  }) {});

  # doctests: doctests: could not execute: markdown-unlit
  # Test suite doctests: FAIL
  rank2classes = dontCheck super.rank2classes;
}
