{ lib, haskellLib, nixpkgs, fetchgit, fetchFromGitHub, useReflexOptimizer }:

with haskellLib;

self: super: {
  _dep = super._dep or {} // {
    ghcjsBaseSrc = fetchgit {
      url = "https://github.com/ghcjs/ghcjs-base.git";
      rev = "6be0e992e292db84ab42691cfb172ab7cd0e709e";
      sha256 = "0nk7a01lprf40zsiph3ikwcqcdb1lghlj17c8zzhiwfmfgcc678g";
    };
  };

  ghcWithPackages = selectFrom: self.callPackage (nixpkgs.path + "/pkgs/development/haskell-modules/with-packages-wrapper.nix") {
    inherit (self) llvmPackages;
    packages = selectFrom self;
  } // nixpkgs.lib.optionalAttrs useReflexOptimizer {
    ghcLibdir = "${self.ghc.bootPackages.ghcWithPackages (p: [ p.reflex ])}/lib/${self.ghc.bootPackages.ghc.name}";
  };

  ghcjs-base = doJailbreak (dontCheck (self.callCabal2nix "ghcjs-base" ghcjsBaseSrc {}));

  ghc = if !(lib.versionAtLeast super.ghc.ghcVersion "8.2") then super.ghc else super.ghc.overrideAttrs (_: {
    # TODO: I don't think this is needed except for maybe the fast-weak patch, but doing this to preserve hashes.
    phases = [ "unpackPhase" "patchPhase" "buildPhase" ];
  }) // {
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
