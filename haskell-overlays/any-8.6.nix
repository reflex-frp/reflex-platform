{ pkgs, haskellLib, fetchFromGitHub }:
with haskellLib;
self: super: {
  cabal-macosx = dontCheck super.cabal-macosx;
  haddock-library-ghcjs = dontCheck super.haddock-library-ghcjs;
  haddock-api-ghcjs = dontCheck super.haddock-api-ghcjs;
  ghc-lib-parser = self.callHackage "ghc-lib-parser" "8.10.2.20200808" {};
  hlint = self.callHackage "hlint" "3.2.8" {};
  http-date = dontCheck super.http-date;
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
  cabal-install = (self.callHackage "cabal-install" "2.4.1.0" {}).override {
    Cabal = self.Cabal_2_4_1_0;
    base16-bytestring = self.base16-bytestring_0_1_1_7;
    hackage-security = haskellLib.dontCheck (haskellLib.doJailbreak
      ((self.callHackage "hackage-security" "0.5.3.0" {}).override {
        base16-bytestring = self.base16-bytestring_0_1_1_7;
        Cabal = self.Cabal_2_4_1_0;
      }));
  };
}
