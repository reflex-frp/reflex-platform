{ haskellLib, nixpkgs, fetchFromGitHub, useReflexOptimizer, hackGet, ghcjsBaseSrc }:

let inherit (nixpkgs) lib; in
with haskellLib;

self: super: {
  ghcWithPackages = selectFrom: self.callPackage (nixpkgs.path + "/pkgs/development/haskell-modules/with-packages-wrapper.nix") {
    inherit (self) llvmPackages;
    packages = selectFrom self;
  } // nixpkgs.lib.optionalAttrs useReflexOptimizer {
    ghcLibdir = "${self.ghc.bootPackages.ghcWithPackages (p: [ p.reflex ])}/lib/${self.ghc.bootPackages.ghc.name}";
  };

  ghcjs-base = doJailbreak (dontCheck (self.callCabal2nix "ghcjs-base" ghcjsBaseSrc {}));

  ghc = super.ghc // {
    withPackages = self.ghcWithPackages;
  };

  # doctest doesn't work on ghcjs, but sometimes dontCheck doesn't seem to get rid of the dependency
  doctest = lib.warn "ignoring dependency on doctest" null;

  # These packages require doctest
  comonad = dontCheck super.comonad;
  http-types = dontCheck super.http-types;
  lens = disableCabalFlag (disableCabalFlag (dontCheck super.lens) "test-properties") "test-doctests";
  pgp-wordlist = dontCheck super.pgp-wordlist;
  prettyprinter = dontCheck super.prettyprinter;
  semigroupoids = disableCabalFlag super.semigroupoids "doctests";
  these = dontCheck super.these;

  # Convenience: tests take long to finish
  megaparsec = dontCheck super.megaparsec;

  # Need newer version of colour for some reason.
  colour = dontCheck (super.colour.overrideAttrs (drv: {
    src = nixpkgs.buildPackages.fetchurl {
      url = "http://hackage.haskell.org/package/colour-2.3.4/colour-2.3.4.tar.gz";
      sha256 = "1sy51nz096sv91nxqk6yk7b92b5a40axv9183xakvki2nc09yhqg";
    };
  }));
}
