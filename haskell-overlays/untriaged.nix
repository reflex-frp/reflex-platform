{ haskellLib
, fetchFromGitHub
, enableLibraryProfiling
}:

with haskellLib;

self: super: {
  language-nix = dontCheck super.language-nix;
  hasktags = dontCheck super.hasktags;
  http-reverse-proxy = dontCheck super.http-reverse-proxy;
  xmlhtml = dontCheck super.xmlhtml;
  haven = doJailbreak super.haven;
  mmorph = doJailbreak super.mmorph;
  async = self.callHackage "async" "2.2.1" {};
  lifted-async = self.callHackage "lifted-async" "0.10.0.2" {};
  hinotify = self.callHackage "hinotify" "0.3.10" {};
  fsnotify = self.callHackage "fsnotify" "0.3.0.1" {};

  ########################################################################
  # Packages not in hackage
  ########################################################################
  servant-reflex = self.callCabal2nix "servant-reflex" (fetchFromGitHub {
    owner = "imalsogreg";
    repo = "servant-reflex";
    rev = "5cd3098880741e6ade52ef4477422d9c776e5478";
    sha256 = "18yjfamx3k9xd8pz251jsmvhlj4riw0brk2fyvjq00r87cx67a6f";
  }) {};
  concat = dontHaddock (dontCheck (self.callCabal2nix "concat" (fetchFromGitHub {
    owner = "conal";
    repo = "concat";
    rev = "24a4b8ccc883605ea2b0b4295460be2f8a245154";
    sha256 = "0mcwqzjk3f8qymmkbpa80l6mh6aa4vcyxky3gpwbnx19g721mj35";
  }) {}));
  direct-sqlite = self.callCabal2nix "direct-sqlite" (fetchFromGitHub {
    owner = "IreneKnapp";
    repo = "direct-sqlite";
    rev = "8e3da41c46b5de19942cc7bf421c3deb5117ba7a";
    sha256 = "0ffk5j1db2y1drn0przh4jw9gc3vygwd987wl1g1m3dw7ry4dxy6";
  }) {};

  a = self.callCabal2nix "a" (../a) {};

  mkDerivation = expr: super.mkDerivation (expr // {
    inherit enableLibraryProfiling;
  });
}
