{ haskellLib
, fetchFromGitHub
, nixpkgs
}:
with haskellLib;
let # Adds additional arguments to 'buildInputs' and the 'HASKELL_GI_GIR_SEARCH_PATH' environment variable
    # used by haskell-gi to specify non-standard locations .gir file locations
    # addGIDeps :: haskellPackage -> [nixPackage] -> [nixPackage] -> haskellPackage
    addGIDeps = p: extraBuildInputs: girSearchPathPackages: p.overrideAttrs (drv: {
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

  # Recently uploaded to hackage:
  haven = self.callHackage "haven" "0.2.0.2" {};

  # Update hlint and add new dependency
  hlint = self.callHackage "hlint" "2.2.2" {};
  ghc-lib-parser = self.callHackage "ghc-lib-parser" "8.8.0.20190723" {};
  haskell-src-exts-util = self.callHackage "haskell-src-exts-util" "0.2.5" {};
  stylish-haskell = self.callHackage "stylish-haskell" "0.9.2.2" {};

  # Fixing things that are marked broken in 19.03:
  butcher = doJailbreak (self.callHackage "butcher" "1.3.2.3" {});
  multistate = self.callHackage "multistate" "0.8.0.2" {};
  haddock-api = dontHaddock (doJailbreak (self.callHackage "haddock-api" "2.22.0" {}));
  constrained-dynamic = self.callHackage "constrained-dynamic" "0.1.0.0" {};
  hsimport = doJailbreak (self.callHackage "hsimport" "0.10.0" {});
  webkit2gtk3-javascriptcore = self.callHackage "webkit2gtk3-javascriptcore" "0.14.2.1" {};
  haskell-gi = self.callHackage "haskell-gi" "0.22.6" {};

  # Snap and deps are marked broken in 19.03 but needed by obelisk
  snap = self.callHackage "snap" "1.1.2.0" {};
  heist = dontCheck (self.callHackage "heist" "1.1.0.1" {});
  map-syntax = doJailbreak (self.callHackage "map-syntax" "0.3" {});

  # Fixing things that are marked broken in 19.09:
  brittany = dontCheck (self.callHackage "brittany" "0.12.0.0" {});
  witherable = self.callHackage "witherable" "0.3.1" {};
  time-compat = dontCheck super.time-compat;
  bimap = self.callHackage "bimap" "0.3.3" {};

  # Overrides for gi-* family of libraries. See addGIDeps, above.
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

  # Packages not yet in 19.03
  semialign = self.callHackage "semialign" "1.1" {};
  these = self.callHackage "these" "1.0.1" {};
  semialign-indexed = self.callHackage "semialign-indexed" "1.1" {}; # to work with semialign 1.1
  these-lens = doJailbreak (self.callHackage "these-lens" "1" {});
  # remove jailbreak after https://github.com/isomorphism/these/pull/134
  which = self.callHackage "which" "0.1.0.0" {};

  # Broken in 19.09
  http-streams = doJailbreak (self.callHackage "http-streams" "0.8.6.1" {});

  # pandoc 2.11 and dependencies
  commonmark = self.callHackageDirect {
    pkg = "commonmark";
    ver = "0.1.0.2";
    sha256 = "sha256:12r65r3dbqwwbr698zvwjcgbp5r68zzd2w2ycp6iz1lv094l49l9";
  } {};
  commonmark-extensions = self.callHackageDirect {
    pkg = "commonmark-extensions";
    ver = "0.2.0.1";
    sha256 = "sha256:13zyf9vqi41bv0cdxr6y2lx9yvq7ca0m0qp4q7jajagd8l6xvqby";
  } {};
  commonmark-pandoc = self.callHackageDirect {
    pkg = "commonmark-pandoc";
    ver = "0.2.0.1";
    sha256 = "sha256:0a3has4ijw499rfmcq5ck59s4x73zz56gm1dans96d9qx069pjl0";
  } {};
  citeproc = doJailbreak (self.callHackageDirect {
    pkg = "citeproc";
    ver = "0.1.0.2";
    sha256 = "sha256:0ia0ah718icl1ahwchqvq56kl96m8xxmkck2lkn303139s2fxarq";
  } {});
  emojis = self.callHackageDirect {
    pkg = "emojis";
    ver = "0.1";
    sha256 = "sha256:0lwj0vm3wfjh437ssfrf5zplpm9a838wi5w170csjjw7chp6qli5";
  } {};
  jira-wiki-markup = self.callHackageDirect {
    pkg = "jira-wiki-markup";
    ver = "1.3.2";
    sha256 = "sha256:063v26pdahz2nd6g7w3j5zidf6n3sd1rgns5jv52scky626jrp6j";
  } {};
  pandoc = self.callHackageDirect {
    pkg = "pandoc";
    ver = "2.11.0.2";
    sha256 = "sha256:0kpqvm0gf3mws1azh8a6r1kb26khklsawr47gcqpfw599d4bxhz7";
  } {};
  pandoc-types = self.callHackageDirect {
    pkg = "pandoc-types";
    ver = "1.22";
    sha256 = "sha256:1ldqp52zrgwdxw67m860g3hkn5zb83cmm0j4cqm36rivgkp3z99c";
  } {};
  texmath = self.callHackageDirect {
    pkg = "texmath";
    ver = "0.12.0.3";
    sha256 = "sha256:1abjr6cldsqm6q0v3q2zvy5frhc4hjp6c6h8pf67yqzkplr02av4";
  } {};
  rfc5051 = self.callHackageDirect {
    pkg = "rfc5051";
    ver = "0.2";
    sha256 = "sha256:1zv3ksy8crpq30f4agk7j7khx5lljjpjalfwyhaznhyg2ax5bbf8";
  } {};
  HsYAML = self.callHackageDirect {
    pkg = "HsYAML";
    ver = "0.2.1.0";
    sha256 = "sha256:0r2034sw633npz7d2i4brljb5q1aham7kjz6r6vfxx8qqb23dwnc";
  } {};
  connection = self.callHackageDirect {
    pkg = "connection";
    ver = "0.3.1";
    sha256 = "sha256:0qjdz2fxxszbns7cszhnkwm8x8l3xlnad6iydx2snfi416sypiy0";
  } {};
  doclayout = self.callHackageDirect {
    pkg = "doclayout";
    ver = "0.3";
    sha256 = "sha256:1si707v469rc43sxgkp2x27n1bsdj29vb18s4czjbbqc14ri8ng1";
  } {};
  doctemplates = self.callHackageDirect {
    pkg = "doctemplates";
    ver = "0.8.2";
    sha256 = "sha256:1idmqpc90b9ifpl1gy3ixi0ny6kqi1vxg033zkyl55fa4jykksn3";
  } {};
  haddock-library = doJailbreak (self.callHackageDirect {
    pkg = "haddock-library";
    ver = "1.8.0";
    sha256 = "sha256:1hmfrfygazdkyxxgh2n2a0ff38c8p4bnlxpk9gia90jn0c5im2n5";
  } {});
  hslua  = self.callHackageDirect {
    pkg = "hslua";
    ver = "1.1.0";
    sha256 = "sha256:1fph7qa5pfaz3x68qj78s5kccam96abddvc3wsfpcp2ldsz9bh7x";
  } {};
  skylighting = self.callHackageDirect {
    pkg = "skylighting";
    ver = "0.10.0.2";
    sha256 = "sha256:1nhsvbmhib8iddsykvlf7rp77qgbx2mk71qh9skhd0454yinwv2n";
  } {};
  skylighting-core = self.callHackageDirect {
    pkg = "skylighting-core";
    ver = "0.10.0.2";
    sha256 = "sha256:0a2bgpwf6y54hlrqcy3ry02in6cdik79y69pllsadbdvc7vddcfb";
  } {};


}
