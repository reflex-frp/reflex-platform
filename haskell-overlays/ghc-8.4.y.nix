{ pkgs, haskellLib, fetchFromGitHub }:
with haskellLib;
self: super: {
  cabal-macosx = dontCheck super.cabal-macosx;
  enclosed-exceptions = dontCheck super.enclosed-exceptions; # see https://github.com/jcristovao/enclosed-exceptions/issues/12
  haddock-library-ghcjs = dontCheck super.haddock-library-ghcjs;
  haddock-api-ghcjs = dontCheck super.haddock-api-ghcjs;

  # Versions that haven’t made it into the Haskell LTS release yet
  testing-feat = super.testing-feat_1_1_0_0;
  snap = super.snap_1_1_1_0;
  snap-server = super.snap-server_1_1_0_0;
  heist = dontCheck super.heist_1_1;
  map-syntax = super.map-syntax_0_3;
  logging-effect = super.logging-effect_1_3_1;

  algebraic-graphs = doJailbreak super.algebraic-graphs;

  # Broken in master
  # PR is https://github.com/vincenthz/hit/pull/37
  hit = dontCheck (self.callCabal2nix "hit" (fetchFromGitHub {
    owner = "vincenthz";
    repo = "hit";
    rev = "e93b01a295d5b4ca51b32b928b37ae040366e317";
    sha256 = "1vfxqc3kffls11dzxq0gk62ky8rjm455cnh0nv31x43g3pmhh7sp";
  }) {});
}
