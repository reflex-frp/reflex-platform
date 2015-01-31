let nixpkgs = import ./nixpkgs {};
    extendHaskellPackages = haskellPackages: haskellPackages.override {
      extension = self: super: {
        reflex = self.callPackage ./reflex {};
        reflexDom = self.callPackage ./reflex-dom {};
        reflexTodomvc = self.callPackage ./reflex-todomvc {};
      };
    };
in {
  inherit nixpkgs;
  haskellPackages_ghc784 = extendHaskellPackages nixpkgs.pkgs.haskellPackages_ghc784_profiling;
  haskellPackages_ghcjs = extendHaskellPackages nixpkgs.pkgs.haskellPackages_ghcjs;
}
