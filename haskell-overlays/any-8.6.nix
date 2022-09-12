{ pkgs, haskellLib, fetchFromGitHub }:
with haskellLib;
self: super: {
  cabal-macosx = dontCheck super.cabal-macosx;
  haddock-library-ghcjs = dontCheck super.haddock-library-ghcjs;
  haddock-api-ghcjs = dontCheck super.haddock-api-ghcjs;
  ghc-lib-parser = self.callHackage "ghc-lib-parser" "8.10.2.20200808" {};
  ghc-lib-parser-ex = self.callHackage "ghc-lib-parser-ex" "8.10.0.16" {};
  hlint = self.callHackage "hlint" "3.1.6" {};
  resolv = super.resolv.override {
    base16-bytestring = self.base16-bytestring_0_1_1_7;
  };
  cabal2nix = super.cabal2nix.override { 
    distribution-nixpkgs = self.distribution-nixpkgs-8_6;
  };
  distribution-nixpkgs-8_6 = super.distribution-nixpkgs_1_6_0.override {
    Cabal = super.Cabal_3_2_1_0;                                                            
  };
  cabal-install = super.cabal-install.override {
    Cabal = super.Cabal_3_4_0_0;
    random = super.random_1_2_0;
  };
}
