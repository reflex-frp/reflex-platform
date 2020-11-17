self: _: {

  all-cabal-hashes = self.fetchurl {
    url = https://github.com/commercialhaskell/all-cabal-hashes/archive/f8af2fab33ba6cfe1984b5c1b3aafac8764ec412.tar.gz;
    sha256 = "07g7b7y34kxmz0lyv7pziyjgfmy56swymxj0qh123v6lnw6rnkg2";
  };

}
