self: _: {

  all-cabal-hashes = self.fetchurl {
    url = https://github.com/commercialhaskell/all-cabal-hashes/archive/75d1bbbd68d9ae36d527666e2f140343323b02fa.tar.gz;
    sha256 = "018lz90f12bw8n8g4rbwfbnpmw6g3vblc12pd4qzq727h1d27p5l";
  };

}
