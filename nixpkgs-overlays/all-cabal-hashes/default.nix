self: _: {

  all-cabal-hashes = self.fetchurl {
    url = https://github.com/commercialhaskell/all-cabal-hashes/archive/02611fecba726dceab2ba02079ae0e6ed86efa4d.tar.gz;
    sha256 = "1n0rizsylq4l3m81n7x339cc75p6vzg0jnz7qq4y8a3fkjqcfwb9";
  };

}
