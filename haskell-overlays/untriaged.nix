{ haskellLib
, fetchFromGitHub
, enableLibraryProfiling
, nixpkgs
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
  hinotify = self.callHackage "hinotify" "0.3.10" {};
  fsnotify = self.callHackage "fsnotify" "0.3.0.1" {};
  hlint = doJailbreak super.hlint;

  # Fixing things that are marked broken in 19.03:
  brittany = self.callHackage "brittany" "0.12.0.0" {};
  butcher = doJailbreak (self.callHackage "butcher" "1.3.2.3" {});
  multistate = self.callHackage "multistate" "0.8.0.2" {};
  haddock-api = dontHaddock (doJailbreak (self.callHackage "haddock-api" "2.22.0" {}));
  constrained-dynamic = self.callHackage "constrained-dynamic" "0.1.0.0" {};
  hsimport = doJailbreak (self.callHackage "hsimport" "0.10.0" {});
  webkit2gtk3-javascriptcore = self.callHackage "webkit2gtk3-javascriptcore" "0.14.2.1" {};
  haskell-gi = self.callHackage "haskell-gi" "0.22.6" {};

  # Overrides for haskell-gi
  haskell-gi-base = (self.callHackage "haskell-gi-base" "0.22.2" {}).overrideAttrs (drv: {
    # cabal2nix puts these deps in libraryPkgconfigDepends but that doesn't seem to suffice.
    buildInputs = drv.buildInputs or [] ++ [ nixpkgs.pkgconfig nixpkgs.glib ];
  });
  # Required by haskell-gi
  gi-cairo = (self.callHackage "gi-cairo" "1.0.19" {}).overrideAttrs (drv: {
    # cabal2nix puts these deps in libraryPkgconfigDepends but that doesn't seem to suffice.
    buildInputs = drv.buildInputs or [] ++ [ nixpkgs.pkgconfig nixpkgs.cairo nixpkgs.glib ];
  });

  # TODO https://github.com/NixOS/cabal2nix/commit/426fde8847370c32731a1db314283f5ebcbabeb7

  # gi-javascriptcore = self.callHackage "gi-javascriptcore" "4.0.20" {};
  # gi-glib = self.callHackage "gi-glib" "2.0.19" {};
  # Required by butcher
  deque = self.callHackage "deque" "0.4.2.3" {};
  strict-list = self.callHackage "strict-list" "0.1.4" {};

  # These take over an hour to run, each
  cryptonite = dontCheck super.cryptonite;
  scientific = dontCheck super.scientific;

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
