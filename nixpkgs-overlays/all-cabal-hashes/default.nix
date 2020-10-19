self: _: {

  all-cabal-hashes = self.fetchurl {
    url = https://github.com/commercialhaskell/all-cabal-hashes/archive/24708a89c02c2abbb056db79310d153c9b657f13.tar.gz;
    sha256 = "14myw2wr1lx8gb7vsc3qfr10f3kl8x4dm9g2d70f8r3h704k48p1";
  };

}
