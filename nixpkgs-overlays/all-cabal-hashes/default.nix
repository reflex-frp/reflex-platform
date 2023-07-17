self: _: {

  all-cabal-hashes = self.fetchurl {
    url = https://github.com/commercialhaskell/all-cabal-hashes/archive/f297f1f3fc337e2f94783f254cb6db1ee2022adb.tar.gz;
    sha256 = "sha256-j2HovXCSBkPircuu6N0ZVTIFUTvET7UC4eaiRITrk2g=";
  };
}
