{ pkgs, haskellLib, fetchFromGitHub }:
with haskellLib;
let
  nixpkgs = (import ../nixpkgs/thunk.nix);
in
self: super: {
  inherit nixpkgs;
  cabal-macosx = dontCheck super.cabal-macosx;
  basement = self.callHackage "basement" "0.0.11" { };
  foundation = self.callHackage "foundation" "0.0.25" { };
  Cabal-syntax = super.Cabal-syntax.override {
    Cabal = super.Cabal_3_2_1_0;
  };
  mono-traversable = self.callHackage "mono-traversable" "1.0.15.1" { };
  colour = haskellLib.overrideCabal (self.callHackage "colour" "2.3.5" { }) (drv: {
    doCheck = false;
    testHaskellDepends = [ ];
  });
  c2hs = self.callHackage "c2hs" "0.28.7" {};
  haddock-library-ghcjs = dontCheck super.haddock-library-ghcjs;
  haddock-api-ghcjs = dontCheck super.haddock-api-ghcjs;
  ghc-lib-parser = self.callHackage "ghc-lib-parser" "8.10.7.20220219" { };
  ghc-lib-parser-ex = self.callHackage "ghc-lib-parser-ex" "8.10.0.24" { };
  hlint = self.callHackage "hlint" "3.2.8" { };
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
      preConfigure = ''
        sed -i 's/if os(darwin) || os(freebsd)/if os(linux)/g' *.cabal
      '';
      configureFlags = (drv.configureFlags or [ ]) ++ [
        #"-v3"
      ];
      libraryPkgconfigDepends = [ pkgs.webkitgtk ];
    });
  resolv = haskellLib.doJailbreak (super.resolv.override {
    base16-bytestring = super.base16-bytestring_0_1_1_7;
  });
  cabal2nix = super.cabal2nix.override {
    distribution-nixpkgs = self.distribution-nixpkgs-8_6;
  };
  distribution-nixpkgs-8_6 = super.distribution-nixpkgs_1_6_0.override {
    Cabal = super.Cabal_3_2_1_0;
  };
  reflex-dom-core = haskellLib.dontCheck (super.reflex-dom-core);
  cabal-install = (self.callHackage "cabal-install" "2.4.1.0" { }).override {
    Cabal = self.Cabal_2_4_1_0;
    base16-bytestring = self.base16-bytestring_0_1_1_7;
    hackage-security = haskellLib.dontCheck (haskellLib.doJailbreak
      ((self.callHackage "hackage-security" "0.5.3.0" { }).override {
        base16-bytestring = self.base16-bytestring_0_1_1_7;
        Cabal = self.Cabal_2_4_1_0;
      }));
  };
}
