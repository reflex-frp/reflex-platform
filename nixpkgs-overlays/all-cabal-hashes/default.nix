self: _: {

  all-cabal-hashes = self.fetchurl {
    url = https://github.com/commercialhaskell/all-cabal-hashes/archive/5ea2bb9f4468a4043ed2d878fb6b8a3e70c7436f.tar.gz;
    sha256 = "1pg9vfsdk18ga38vjwl848pxfs81xsgg9rjgd5yvrychczhwr452";
  };

}
