{ haskellLib
, fetchFromGitHub
, enableLibraryProfiling
, nixpkgs
}:
with haskellLib;
let addGIDeps = p: extraBuildInputs: girSearchPathPackages: p.overrideAttrs (drv: {
      # cabal2nix puts these deps in libraryPkgconfigDepends but that doesn't seem to suffice.
      buildInputs = with nixpkgs; drv.buildInputs or [] ++ [ pkgconfig gobjectIntrospection ] ++ extraBuildInputs;
      libraryPkgconfigDepends = drv.libraryPkgconfigDepends or [] ++ [nixpkgs.gobject-introspection];
      # This preConfigure should have been added by cabal2nix according to this commit: https://github.com/NixOS/cabal2nix/commit/426fde8847370c32731a1db314283f5ebcbabeb7
      # though that functionality was removed in a later commit: https://github.com/NixOS/cabal2nix/commit/2d278a8a1527b278939ba478fe915aa2f87cc22e#diff-387ec31295a66a4f73b8d4b507a239a2
      # Cf. https://github.com/haskell-gi/haskell-gi/issues/36
      preConfigure = "export HASKELL_GI_GIR_SEARCH_PATH=" +
        nixpkgs.lib.concatStringsSep ":"
          (map (x: "${x.dev}/share/gir-1.0") ([nixpkgs.gobjectIntrospection] ++ girSearchPathPackages));
    });
in self: super: {

  # TODO
  reflex-dom-core = dontCheck super.reflex-dom-core;
  reflex-dom = doJailbreak super.reflex-dom;
  jsaddle-webkit2gtk = doJailbreak super.jsaddle-webkit2gtk;

  # Recently uploaded to hackage:
  haven = self.callHackageDirect {
    pkg = "haven";
    ver = "0.2.0.2";
    sha256 = "1hz0ngzd2gbmi45lv52465zrvsfvm6gpc42g7ms0hpa0v52if0w9";
  } {};

  language-nix = dontCheck super.language-nix;
  hasktags = dontCheck super.hasktags;
  http-reverse-proxy = dontCheck super.http-reverse-proxy;
  xmlhtml = dontCheck super.xmlhtml;
  mmorph = doJailbreak super.mmorph;
  async = self.callHackage "async" "2.2.1" {};
  hinotify = self.callHackage "hinotify" "0.3.10" {};
  fsnotify = self.callHackage "fsnotify" "0.3.0.1" {};

  # Update hlint and add new dependency
  hlint = self.callHackage "hlint" "2.1.26" {};
  ghc-lib-parser = self.callHackage "ghc-lib-parser" "8.8.0.20190424" {};
  haskell-src-exts = super.haskell-src-exts_1_21_0;
  haskell-src-exts-util = self.callHackage "haskell-src-exts-util" "0.2.5" {};
  stylish-haskell = self.callHackage "stylish-haskell" "0.9.2.2" {};

  # Fixing things that are marked broken in 19.03:
  brittany = self.callHackage "brittany" "0.12.0.0" {};
  butcher = doJailbreak (self.callHackage "butcher" "1.3.2.3" {});
  multistate = self.callHackage "multistate" "0.8.0.2" {};
  haddock-api = dontHaddock (doJailbreak (self.callHackage "haddock-api" "2.22.0" {}));
  constrained-dynamic = self.callHackage "constrained-dynamic" "0.1.0.0" {};
  hsimport = doJailbreak (self.callHackage "hsimport" "0.10.0" {});
  webkit2gtk3-javascriptcore = self.callHackage "webkit2gtk3-javascriptcore" "0.14.2.1" {};
  haskell-gi = self.callHackage "haskell-gi" "0.22.6" {};

  # Overrides for gi-* family of libraries
  haskell-gi-base = addGIDeps (self.callHackage "haskell-gi-base" "0.22.2" {}) [nixpkgs.glib] [];
  gi-glib = addGIDeps (self.callHackage "gi-glib" "2.0.19" {}) [] [];
  gi-cairo = addGIDeps (self.callHackage "gi-cairo" "1.0.19" {}) [nixpkgs.cairo] [];
  gi-gobject = addGIDeps (self.callHackage "gi-gobject" "2.0.21" {}) [] [];
  gi-pango = addGIDeps (self.callHackage "gi-pango" "1.0.21" {}) [nixpkgs.pango] [];
  gi-gio = addGIDeps (self.callHackage "gi-gio" "2.0.24" {}) [] [];
  gi-atk = addGIDeps (self.callHackage "gi-atk" "2.0.20" {}) [] [];
  gi-javascriptcore = addGIDeps (self.callHackage "gi-javascriptcore" "4.0.20" {}) [] [];
  gi-gdkpixbuf = addGIDeps (self.callHackage "gi-gdkpixbuf" "2.0.22" {}) [nixpkgs.gdk_pixbuf nixpkgs.gtk3] [nixpkgs.gtk3];
  gi-gdk = addGIDeps (self.callHackage "gi-gdk" "3.0.21" {}) [nixpkgs.gdk_pixbuf nixpkgs.pango nixpkgs.gtk3] [nixpkgs.gtk3];
  gi-soup = addGIDeps (self.callHackage "gi-soup" "2.4.21" {}) [nixpkgs.gdk_pixbuf] [nixpkgs.libsoup];
  gi-gtk = addGIDeps (self.callHackage "gi-gtk" "3.0.31" {}) [nixpkgs.gdk_pixbuf nixpkgs.gtk3] [nixpkgs.gtk3 nixpkgs.atk nixpkgs.pango];
  gi-webkit2 = addGIDeps (self.callHackage "gi-webkit2" "4.0.24" {}) [] [nixpkgs.webkitgtk];

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
