{ haskellLib, fetchFromGitHub }:
with haskellLib;
self: super: {
  cabal-macosx = dontCheck super.cabal-macosx;
  haskell-src-exts = self.callHackage "haskell-src-exts" "1.20.2" {};
  haskell-src-meta = overrideCabal super.haskell-src-meta (drv: {
    revision = "1";
    editedCabalFile = "07xxp2r8amd420bzl7xlyfydhyrcrvaxq24ydnx7y2sz4v2nlzsz";
  });
  enclosed-exceptions = dontCheck super.enclosed-exceptions; # see https://github.com/jcristovao/enclosed-exceptions/issues/12
  haddock-library-ghcjs = dontCheck super.haddock-library-ghcjs;
  haddock-api-ghcjs = dontCheck super.haddock-api-ghcjs;
}
