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
    buildInputs = with nixpkgs; drv.buildInputs or [ ] ++ [ pkgconfig gobjectIntrospection ] ++ extraBuildInputs;
    libraryPkgconfigDepends = drv.libraryPkgconfigDepends or [ ] ++ [ nixpkgs.gobject-introspection ];
    # This preConfigure should have been added by cabal2nix according to this commit: https://github.com/NixOS/cabal2nix/commit/426fde8847370c32731a1db314283f5ebcbabeb7
    # though that functionality was removed in a later commit: https://github.com/NixOS/cabal2nix/commit/2d278a8a1527b278939ba478fe915aa2f87cc22e#diff-387ec31295a66a4f73b8d4b507a239a2
    # Cf. https://github.com/haskell-gi/haskell-gi/issues/36
    preConfigure = "export HASKELL_GI_GIR_SEARCH_PATH=" +
      nixpkgs.lib.concatStringsSep ":"
        (map (x: "${x.dev}/share/gir-1.0") ([ nixpkgs.gobjectIntrospection ] ++ girSearchPathPackages));
  });

  nixpkgsPath_20_03 = import ./nixpkgs-20.03/thunk.nix;

  atk_old = nixpkgs.callPackage "${nixpkgsPath_20_03}/pkgs/development/libraries/atk" { };
  gdk-pixbuf_old = nixpkgs.callPackage "${nixpkgsPath_20_03}/pkgs/development/libraries/gdk-pixbuf" { };
  gtk3_old = nixpkgs.callPackage "${nixpkgsPath_20_03}/pkgs/development/libraries/gtk/3.x.nix" {
    inherit (nixpkgs.darwin.apple_sdk.frameworks) AppKit Cocoa;
    atk = atk_old;
    gdk-pixbuf = gdk-pixbuf_old;
    pango = pango_old;
  };
  libsoup_old = nixpkgs.callPackage "${nixpkgsPath_20_03}/pkgs/development/libraries/libsoup" { };
  pango_old = nixpkgs.callPackage "${nixpkgsPath_20_03}/pkgs/development/libraries/pango" {
    harfbuzz = nixpkgs.harfbuzz.override { withCoreText = nixpkgs.stdenv.isDarwin; };
  };
  webkitgtk_old = nixpkgs.callPackage "${nixpkgsPath_20_03}/pkgs/development/libraries/webkitgtk" {
    harfbuzz = nixpkgs.harfbuzzFull;
    inherit (nixpkgs.gst_all_1) gst-plugins-base gst-plugins-bad;
    stdenv = nixpkgs.clangStdenv; # TODO: https://github.com/NixOS/nixpkgs/issues/36947
    gtk3 = gtk3_old;
    libsoup = libsoup_old;
  };
in
self: super: {

  # Overrides for gi-* family of libraries. See addGIDeps, above.
  # Also use an older version suitable for GHC 8.6, because haskell-gi-base ==
  # 0.24.2 needs a newer compiler. https://github.com/haskell-gi/haskell-gi/issues/304
  # We also need nixpkgs 20.03 for older gi* deps. TODO: This should be removed
  # after GHC is bumped, and the uses of (callHackage "x") can be switched back
  # to (super.x).
  haskell-gi-base = addGIDeps (self.callHackage "haskell-gi-base" "0.23.0" { }) [ nixpkgs.glib ] [ ];
  haskell-gi = addGIDeps (self.callHackage "haskell-gi" "0.23.0" { }) [ ] [ ];
  gi-glib = addGIDeps (self.callHackage "gi-glib" "2.0.23" { }) [ ] [ ];
  gi-cairo = addGIDeps (self.callHackage "gi-cairo" "1.0.23" { }) [ nixpkgs.cairo ] [ ];
  gi-gobject = addGIDeps (self.callHackage "gi-gobject" "2.0.22" { }) [ ] [ ];
  gi-pango = addGIDeps (self.callHackage "gi-pango" "1.0.22" { }) [ pango_old ] [ ];
  gi-gio = addGIDeps (self.callHackage "gi-gio" "2.0.25" { }) [ ] [ ];
  gi-atk = addGIDeps (self.callHackage "gi-atk" "2.0.21" { }) [ ] [ ];
  gi-javascriptcore = addGIDeps (self.callHackage "gi-javascriptcore" "4.0.21" { }) [ ] [ ];
  gi-gdkpixbuf = addGIDeps (self.callHackage "gi-gdkpixbuf" "2.0.23" { }) [ gdk-pixbuf_old gtk3_old ] [ gtk3_old ];
  gi-gdk = addGIDeps (self.callHackage "gi-gdk" "3.0.22" { }) [ gdk-pixbuf_old pango_old gtk3_old ] [ gtk3_old ];
  gi-soup = addGIDeps (self.callHackage "gi-soup" "2.4.22" { }) [ gdk-pixbuf_old ] [ libsoup_old ];
  gi-gtk = addGIDeps (self.callHackage "gi-gtk" "3.0.32" { }) [ gdk-pixbuf_old gtk3_old ] [ gtk3_old atk_old pango_old ];
  gi-webkit2 = addGIDeps (self.callHackage "gi-webkit2" "4.0.25" { }) [ ] [ webkitgtk_old ];

}
