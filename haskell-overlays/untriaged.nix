{ haskellLib
, lib, nixpkgs
, fetchFromGitHub
, enableLibraryProfiling
}:

with haskellLib;

self: super: {
  haven = self.callHackage "haven" "0.2.0.0" {};
  base-compat = self.callHackage "base-compat" "0.9.2" {};
  constraints = self.callHackage "constraints" "0.9" {};
  servant-auth-server = self.callHackage "servant-auth-server" "0.3.1.0" {};
  vector = doJailbreak super.vector;
  these = doJailbreak super.these;
  aeson-compat = doJailbreak super.aeson-compat;
  timezone-series = self.callCabal2nix "timezone-series" (fetchFromGitHub {
    owner = "ygale";
    repo = "timezone-series";
    rev = "9f42baf542c54ad554bd53582819eaa454ed633d";
    sha256 = "1axrx8lziwi6pixws4lq3yz871vxi81rib6cpdl62xb5bh9y03j6";
  }) {};
  timezone-olson = self.callCabal2nix "timezone-olson" (fetchFromGitHub {
    owner = "ygale";
    repo = "timezone-olson";
    rev = "aecec86be48580f23145ffb3bf12a4ae191d12d3";
    sha256 = "1xxbwb8z27qbcscbg5qdyzlc2czg5i3b0y04s9h36hfcb07hasnz";
  }) {};
  quickcheck-instances = doJailbreak super.quickcheck-instances;

  haskell-src-meta = self.callHackage "haskell-src-meta" "0.8.0.1" {};
  gtk2hs-buildtools = doJailbreak super.gtk2hs-buildtools;

  # hindent was overriden with a newer version of haskell-src-exts for some reason
  hindent = super.hindent.override { haskell-src-exts = self.haskell-src-exts; };
  # Not sure why these tests fail...
  hfmt = dontCheck super.hfmt;

  ########################################################################
  # Tweaks
  ########################################################################
  gi-glib = self.callPackage ../gi-glib.nix {};
  gi-gio = self.callPackage ../gi-gio.nix {};
  gi-gtk = self.callPackage ../gi-gtk.nix {
    gtk3 = nixpkgs.gnome3.gtk;
  };
  gi-javascriptcore = self.callPackage ../gi-javascriptcore.nix {};
  gi-webkit2 = self.callPackage ../gi-webkit2.nix {
    webkitgtk = nixpkgs.webkitgtk216x;
  };
  gi-gtksource = super.gi-gtksource.override {
    inherit (nixpkgs.gnome3) gtksourceview;
  };
  ghcjs-base-stub = dontHaddock super.ghcjs-base-stub;

  haskell-gi-overloading = super.haskell-gi-overloading_0_0;

  webkit2gtk3-javascriptcore = super.webkit2gtk3-javascriptcore.override {
    webkitgtk = nixpkgs.webkitgtk216x;
  };

  cabal-macosx = overrideCabal super.cabal-macosx (drv: {
    src = fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "cabal-macosx";
      rev = "b1e22331ffa91d66da32763c0d581b5d9a61481b";
      sha256 = "1y2qk61ciflbxjm0b1ab3h9lk8cm7m6ln5ranpf1lg01z1qk28m8";
    };
    doCheck = false;
  });

  ########################################################################
  # Fixes to be upstreamed
  ########################################################################
  foundation = dontCheck super.foundation;
  MonadCatchIO-transformers = doJailbreak super.MonadCatchIO-transformers;
  blaze-builder-enumerator = doJailbreak super.blaze-builder-enumerator;
  process-extras = dontCheck super.process-extras;
  miso = addBuildDepend (self.callHackage "miso" "0.12.0.0" {}) self.ghcjs-base;

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
    rev = "cd1ab3c0ee7894d888be826fc653b75813fd53c9";
    sha256 = "13i6lz99x0jb9fgns7brlqnv5s5w4clp26l8c3kxd318r1krvr6w";
  }) {};

  superconstraints =
    # Remove override when assertion fails
    assert (super.superconstraints or null) == null;
    self.callPackage (self.haskellSrc2nix {
      name = "superconstraints";
      src = fetchurl {
        url = "https://hackage.haskell.org/package/superconstraints-0.0.1/superconstraints.cabal";
        sha256 = "0bgc8ldml3533522gp1x2bjiazllknslpl2rvdkd1k1zfdbh3g9m";
      };
      sha256 = "1gx9p9i5jli91dnvvrc30j04h1v2m3d71i8sxli6qrhplq5y63dk";
    }) {};
} // lib.optionalAttrs enableLibraryProfiling {
  mkDerivation = expr: super.mkDerivation (expr // { enableLibraryProfiling = true; });
}
