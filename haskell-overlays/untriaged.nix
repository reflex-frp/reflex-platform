{ haskellLib
, fetchFromGitHub
, enableLibraryProfiling
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
  haven = self.callHackageDirect {
    pkg = "haven";
    ver = "0.2.0.2";
    sha256 = "1hz0ngzd2gbmi45lv52465zrvsfvm6gpc42g7ms0hpa0v52if0w9";
  } {};

  # Update hlint and add new dependency
  hlint = self.callHackageDirect {
    pkg = "hlint";
    ver = "2.2.2";
    sha256 = "0m0mx1cvq2m4w6kf0armfgdayqxwapnf5k8ffjxild8amc8ysn4a";
  } {};
  ghc-lib-parser = self.callHackageDirect {
    pkg = "ghc-lib-parser";
    ver = "8.8.0.20190723";
    sha256 = "0l0ffxmszjrcj8qiqwwmsdygy2sgw6vrlm1xfqrbdx60bwgc07m5";
  } {};
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
  semialign = doJailbreak (self.callHackage "semialign" "1" {});
  these = self.callHackage "these" "1" {};
  these-lens = doJailbreak (self.callHackage "these-lens" "1" {});
  # remove jailbreak after https://github.com/isomorphism/these/pull/134
  which = self.callHackage "which" "0.1.0.0" {};

  ########################################################################
  # Packages not in hackage
  ########################################################################
  concat = dontHaddock (dontCheck (self.callCabal2nix "concat" (fetchFromGitHub {
    owner = "conal";
    repo = "concat";
    rev = "24a4b8ccc883605ea2b0b4295460be2f8a245154";
    sha256 = "0mcwqzjk3f8qymmkbpa80l6mh6aa4vcyxky3gpwbnx19g721mj35";
  }) {}));

  mkDerivation = expr: super.mkDerivation (expr // {
    inherit enableLibraryProfiling;
  });
}
