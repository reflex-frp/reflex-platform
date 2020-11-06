self: _: {

  all-cabal-hashes = self.fetchurl {
    url = https://github.com/commercialhaskell/all-cabal-hashes/archive/9fc1b1a8ef87c10ef0f9f95072b3e00558d98eaa.tar.gz;
    sha256 = "06m0s72xkshq2kyi9fd2pn253ji02jyxll771wjf011wways1917";
  };

}
