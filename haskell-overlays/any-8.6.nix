{ pkgs, haskellLib, fetchFromGitHub }:
with haskellLib;
let
  nixpkgs = (import ../nixpkgs/thunk.nix);
in
self: super: {
  inherit nixpkgs;
  cabal-macosx = dontCheck super.cabal-macosx;
  Cabal-syntax = super.Cabal-syntax.override {
    Cabal = super.Cabal_3_2_1_0;
  };
  colour = haskellLib.overrideCabal (self.callHackage "colour" "2.3.5" { }) (drv: {
    doCheck = false;
    testHaskellDepends = [ ];
  });
  c2hs = self.callHackage "c2hs" "0.28.7" { };
  gtk2hs-buildtools = overrideCabal (self.callHackage "gtk2hs-buildtools" "0.13.8.3" { }) (drv: {
    preConfigure = ''
      sed -i 's|cabal-version:  3.0|cabal-version:  2.0|g' *.cabal
    '';
  });
  webkit2gtk3-javascriptcore = overrideCabal
    (self.callHackageDirect
      {
        pkg = "webkit2gtk3-javascriptcore";
        ver = "0.14.4.4";
        sha256 = "sha256-ZiM2pMigNJZPOxV6zAAjVdCAjg31k2T3oDyZsudkMfg=";
      }
      { })
    (drv: {
      libraryPkgconfigDepends = [ pkgs.webkitgtk ];
    });
  resolv = haskellLib.doJailbreak (super.resolv.override {
    base16-bytestring = super.base16-bytestring_0_1_1_7;
  });
  cabal2nix = super.cabal2nix.override { };
  distribution-nixpkgs_1_6_0 = self.callHackage "distribution-nixpkgs" "1.6.0" { };
  distribution-nixpkgs-8_6 = self.distribution-nixpkgs_1_6_0.override {
    Cabal = super.Cabal_3_2_1_0;
  };
  reflex-dom-core = haskellLib.dontCheck (super.reflex-dom-core);
  cabal-install = haskellLib.doJailbreak ((self.callHackage "cabal-install" "2.4.1.0" { }).override {
    Cabal = self.Cabal_2_4_1_0;
    base16-bytestring = self.base16-bytestring_0_1_1_7;
    #random = self.callHackage "random" "1.1" {};
    hackage-security = haskellLib.dontCheck (haskellLib.doJailbreak
      ((self.callHackage "hackage-security" "0.5.3.0" { }).override {
        base16-bytestring = self.base16-bytestring_0_1_1_7;
        Cabal = self.Cabal_2_4_1_0;
      }));
  });
}
