self: _: {

  all-cabal-hashes = self.fetchurl {
    url = https://github.com/commercialhaskell/all-cabal-hashes/archive/1077d51232196846425da61a7716740513ae6d34.tar.gz;
    sha256 = "1bsdg5rkqp431fyxfh58nrbd57m9qcrlx42nwy12fcybiw7anlf7";
  };

}
