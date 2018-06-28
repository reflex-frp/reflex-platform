{ haskellLib, fetchFromGitHub }:
with haskellLib;
self: super: {
  cabal-macosx = dontCheck super.cabal-macosx;
  enclosed-exceptions = dontCheck super.enclosed-exceptions; # see https://github.com/jcristovao/enclosed-exceptions/issues/12
  haddock-library-ghcjs = dontCheck super.haddock-library-ghcjs;
  haddock-api-ghcjs = dontCheck super.haddock-api-ghcjs;

  testing-feat = dontCheck (doJailbreak super.testing-feat);
  snap-server = self.callCabal2nix "snap-server" (fetchFromGitHub {
    owner = "snapframework";
    repo = "snap-server";
    rev = "b2a888230e107046404b047ff3b8690a592f124c";
    sha256 = "05zbc4lyyphsrkj5h043rgx9gjsgmcd8zahzjz69npd9cf91aa6w";
  }) {};
}
