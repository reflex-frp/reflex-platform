self: _: {

  all-cabal-hashes = self.fetchurl {
    url = https://github.com/commercialhaskell/all-cabal-hashes/archive/0830e8b2bb708de5619928a52044a90e9d3dad29.tar.gz;
    sha256 = "1w7svsx8lvsp421220xnmh312an09gcqxmik72y28cvxk7zi0375";
  };

}
