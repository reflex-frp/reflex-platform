self: _: {

  all-cabal-hashes = self.fetchurl {
    url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/6d2174b2bdc5aff9064795555e23ff318cb00ff6.tar.gz";
    sha256 = "02qa79snhllaba3a9vrxj0j916mpirkw9m3acxvy2ijx2mmp314j";
  };

}
