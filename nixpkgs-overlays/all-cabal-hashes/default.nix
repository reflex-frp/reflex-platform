self: _: {

  all-cabal-hashes = self.fetchurl {
    url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/5ab6d775bc21c819e332114318b7f738c26794f5.tar.gz";
    sha256 = "1565lgias16n03jxjzmh060952xlgbxaa76k4n8d8ynjl2zf1d6k";
  };

}
