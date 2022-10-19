# The build of ghcjs8.6 uses cabal v1 commands, including v1-sdist, so we need an older cabal-install
{ nixpkgs }:
(nixpkgs.haskell.packages.ghc865.callHackage "cabal-install" "2.4.1.0" { }).overrideScope
  (self: super: {
    Cabal = self.Cabal_2_4_1_0;
    base16-bytestring = self.base16-bytestring_0_1_1_7;
    random = self.callHackage "random" "1.1" { };
    hackage-security = nixpkgs.haskell.lib.dontCheck
      (nixpkgs.haskell.lib.doJailbreak
        (self.callHackage "hackage-security" "0.5.3.0" { }));
  })
