{ pkgs, haskellLib, fetchFromGitHub }:
with haskellLib;
self: super: {
  cabal-macosx = dontCheck super.cabal-macosx;
  enclosed-exceptions = dontCheck super.enclosed-exceptions; # see https://github.com/jcristovao/enclosed-exceptions/issues/12
  haddock-library-ghcjs = dontCheck super.haddock-library-ghcjs;
  haddock-api-ghcjs = dontCheck super.haddock-api-ghcjs;

  # Versions that haven’t made it into Nixpkgs master yet
  testing-feat = self.callCabal2nix "testing-feat" (fetchFromGitHub {
    owner = "JonasDuregard";
    repo = "testing-feat";
    rev = "a1b32842a8cc8ab467d93a9f97f2365330a02113";
    sha256 = "1pxafc7rdd25j0m0dxgxqhw3vnxb3lw9gv6n885x2lag9mcdkpax";
  }) {};
  snap = self.callCabal2nix "snap" (fetchFromGitHub {
    owner = "snapframework";
    repo = "snap";
    rev = "2fa933b52d7d126b59d89eddeed0e8a9d58d1d61";
    sha256 = "002byv0iqmxj60c1q8ybnipvaqsjy7j7hv8rd7drdbc2cz422wlh";
  }) {};
  snap-server = self.callCabal2nix "snap-server" (fetchFromGitHub {
    owner = "snapframework";
    repo = "snap-server";
    rev = "b2a888230e107046404b047ff3b8690a592f124c";
    sha256 = "05zbc4lyyphsrkj5h043rgx9gjsgmcd8zahzjz69npd9cf91aa6w";
  }) {};
  heist = addTestToolDepend (self.callCabal2nix "heist" (fetchFromGitHub {
    owner = "snapframework";
    repo = "heist";
    rev = "3ccbec548830abce7ed7eba42c1c294b02b6cd52";
    sha256 = "14sd4d4an7fj8yb4mr8cdallsv69x5jb1hd330sg10ahi1ryzspr";
  }) {}) pkgs.pandoc;
  map-syntax = self.callCabal2nix "map-syntax" (fetchFromGitHub {
    owner = "mightybyte";
    repo = "map-syntax";
    rev = "acebcf0a83ee639e1a0c49850b9c85821d53f621";
    sha256 = "076knpvls1489gish9z30lhb21vqx44k366vc2i3kdql815v1vqv";
  }) {};
  logging-effect = self.callCabal2nix "logging-effect" (fetchFromGitHub {
    owner = "ocharles";
    repo = "logging-effect";
    rev = "bd65c0d0d54f096ab90810b8c3604dd9c110a4ab";
    sha256 = "1zlh8h4gq4kb8j9cgxsaas3h0havq23rrg0pl63d2ii1vghi9181";
  }) {};

  # Broken in master
  # PR is https://github.com/vincenthz/hit/pull/37
  hit = self.callCabal2nix "hit" (fetchFromGitHub {
    owner = "vincenthz";
    repo = "hit";
    rev = "e93b01a295d5b4ca51b32b928b37ae040366e317";
    sha256 = "1vfxqc3kffls11dzxq0gk62ky8rjm455cnh0nv31x43g3pmhh7sp";
  }) {};
}
