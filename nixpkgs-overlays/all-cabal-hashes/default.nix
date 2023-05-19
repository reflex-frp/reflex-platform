self: _: {

  all-cabal-hashes = self.fetchurl {
    url = https://github.com/commercialhaskell/all-cabal-hashes/archive/bac0fa366110399233b0a0820f9944b95ffc58c8.tar.gz;
    sha256 = "sha256-xDvCxka8Aqnw09uRXZMiOlY9pKvhGmSg9hlRjMmIPic=";
  };

}
