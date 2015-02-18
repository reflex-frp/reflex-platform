let nixpkgs = import ./nixpkgs {config.allowUnfree = true;};
    extendHaskellPackages = haskellPackages: haskellPackages.override {
      overrides = self: super: {
        reflex = self.callPackage ./reflex {};
        reflex-dom = self.callPackage ./reflex-dom {};
        reflex-todomvc = self.callPackage ./reflex-todomvc {};
      };
    };
in {
  inherit nixpkgs;
  ghc = extendHaskellPackages nixpkgs.pkgs.haskell-ng.packages.ghc7101;
  ghcjs = extendHaskellPackages nixpkgs.pkgs.haskell-ng.packages.ghcjs;
  platforms = [ "ghcjs" "ghc" ];
}
