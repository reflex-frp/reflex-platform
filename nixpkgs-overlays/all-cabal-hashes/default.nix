self: _: {

  all-cabal-hashes = self.fetchurl {
    url = https://github.com/commercialhaskell/all-cabal-hashes/archive/93fb0665f1ac3cbcfcca1994abea9cf89b97edca.tar.gz;
    sha256 = "0vvh8yy947q08da0nvlgvqqlg7mnwk3qymva8g7rxwva0n4ba3f3";
  };

}
