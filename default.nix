let overrideCabal = drv: f: (drv.override (args: args // {
      mkDerivation = drv: args.mkDerivation (drv // f drv);
    })) // {
      overrideScope = scope: overrideCabal (drv.overrideScope scope) f;
    };
    nixpkgs = import ./nixpkgs {config.allowUnfree = true;};
    extendHaskellPackages = haskellPackages: haskellPackages.override {
      overrides = self: super: {
        reflex = self.callPackage ./reflex {};
        reflex-dom = self.callPackage ./reflex-dom {};
        reflex-todomvc = self.callPackage ./reflex-todomvc {};
        active = overrideCabal super.active (drv: {
          version = "0.1.0.19";
          sha256 = "1zzzrjpfwxzf0zbz8vcnpfqi7djvrfxglhkvw1s6yj5gcblg2rcw";
          doCheck = false;
        });
      };
    };
in rec {
  inherit nixpkgs;
  ghc = extendHaskellPackages nixpkgs.pkgs.haskell-ng.packages.ghc7101;
  ghcjs = extendHaskellPackages nixpkgs.pkgs.haskell-ng.packages.ghcjs;
  platforms = [ "ghcjs" ] ++ (if !nixpkgs.stdenv.isDarwin then [ "ghc" ] else []);
}
