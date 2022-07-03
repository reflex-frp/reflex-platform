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
      buildInputs = with nixpkgs; drv.buildInputs or [] ++ [ pkgconfig gobject-introspection ] ++ extraBuildInputs;
      libraryPkgconfigDepends = drv.libraryPkgconfigDepends or [] ++ [nixpkgs.gobject-introspection];
      # This preConfigure should have been added by cabal2nix according to this commit: https://github.com/NixOS/cabal2nix/commit/426fde8847370c32731a1db314283f5ebcbabeb7
      # though that functionality was removed in a later commit: https://github.com/NixOS/cabal2nix/commit/2d278a8a1527b278939ba478fe915aa2f87cc22e#diff-387ec31295a66a4f73b8d4b507a239a2
      # Cf. https://github.com/haskell-gi/haskell-gi/issues/36
      preConfigure = "export HASKELL_GI_GIR_SEARCH_PATH=" +
        nixpkgs.lib.concatStringsSep ":"
          (map (x: "${x.dev}/share/gir-1.0") ([nixpkgs.gobject-introspection] ++ girSearchPathPackages));
    });
in self: super: {

  # Overrides for gi-* family of libraries. See addGIDeps, above.
  haskell-gi-base = addGIDeps (super.haskell-gi-base) [nixpkgs.glib] [];
  gi-glib = addGIDeps (super.gi-glib) [] [];
  gi-cairo = addGIDeps (super.gi-cairo) [nixpkgs.cairo] [];
  gi-gobject = addGIDeps (super.gi-gobject) [] [];
  gi-pango = addGIDeps (super.gi-pango) [nixpkgs.pango] [];
  gi-gio = addGIDeps (super.gi-gio) [] [];
  gi-atk = addGIDeps (super.gi-atk) [] [];
  gi-javascriptcore = addGIDeps (super.gi-javascriptcore) [] [];
  gi-gdkpixbuf = addGIDeps (super.gi-gdkpixbuf) [nixpkgs.gdk-pixbuf nixpkgs.gtk3] [nixpkgs.gtk3];
  gi-gdk = addGIDeps (super.gi-gdk) [nixpkgs.gdk-pixbuf nixpkgs.pango nixpkgs.gtk3] [nixpkgs.gtk3];
  gi-soup = addGIDeps (super.gi-soup) [nixpkgs.gdk-pixbuf] [nixpkgs.libsoup];
  gi-gtk = addGIDeps (super.gi-gtk) [nixpkgs.gdk-pixbuf nixpkgs.gtk3] [nixpkgs.gtk3 nixpkgs.atk nixpkgs.pango];
  gi-webkit2 = addGIDeps (super.gi-webkit2) [] [nixpkgs.webkitgtk];

}
