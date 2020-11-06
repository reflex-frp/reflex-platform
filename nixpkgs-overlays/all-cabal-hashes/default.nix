self: _: {

  all-cabal-hashes = self.fetchurl {
    url = https://github.com/commercialhaskell/all-cabal-hashes/archive/f9af738d0a5095d79aa4dadfba12435c480c23c8.tar.gz;
    sha256 = "1npwbwf0972bvbwgxf75viqcyak8kapbyx8d54n1ybx77f6cs903";
  };

}
