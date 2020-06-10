self: _: {

  all-cabal-hashes = self.fetchurl {
    url = https://github.com/commercialhaskell/all-cabal-hashes/archive/a47bf76d8dabd429cd50f44761a6bea8f552a93a.tar.gz;
    sha256 = "126xq53kj9sxnf7si6vhr91xx7zr67371ihrpx3cm93ayyv8n25s";
  };

}
