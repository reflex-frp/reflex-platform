{ haskellLib, nixpkgs, fetchFromGitHub, useReflexOptimizer, hackGet }:

with haskellLib;

self: super: {
  ghcWithPackages = selectFrom: self.callPackage (nixpkgs.path + "/pkgs/development/haskell-modules/with-packages-wrapper.nix") {
    inherit (self) llvmPackages;
    packages = selectFrom self;
  } // nixpkgs.lib.optionalAttrs useReflexOptimizer {
    ghcLibdir = "${self.ghc.bootPackages.ghcWithPackages (p: [ p.reflex ])}/lib/${self.ghc.bootPackages.ghc.name}";
  };

  ghcjs-base = overrideCabal (self.callCabal2nix "ghcjs-base" (fetchFromGitHub {
    owner = "ghcjs";
    repo = "ghcjs-base";
    rev = "01014ade3f8f5ae677df192d7c2a208bd795b96c";
    sha256 = "0qr05m0djll3x38dhl85pl798arsndmwfhil8yklhb70lxrbvfrs";
  }) {}) (drv: {
    jailbreak = true;
    doCheck = false; #TODO: This should be unnecessary
    patches = (drv.patches or []) ++ [
      (nixpkgs.fetchpatch {
        url = "https://github.com/ghcjs/ghcjs-base/commit/2d0d674e54c273ed5fcb9a13f588819c3303a865.patch"; #ghcjs-base/114
        sha256 = "15vbxnxa1fpdcmmx5zx1z92bzsxyb0cbs3hs3g7fb1rkds5qbvgp";
      })
      (nixpkgs.fetchpatch {
        url = "https://github.com/ghcjs/ghcjs-base/commit/8eccb8d937041ba323d62dea6fe8eb1b04b3cc47.patch"; #ghcjs-base/116
        sha256 = "1lqjpg46ydpm856wcq1g7c97d69qcnnqs5jxp2b788z9cfd5n64c";
      })
    ];
    #TODO: This should be unnecessary
    preConfigure = (drv.preConfigure or "") + ''
      sed -i -e '/jsbits\/export.js/d' -e '/GHCJS\.Foreign\.Export/d' *.cabal
    '';
  });

  ghc = super.ghc // {
    withPackages = self.ghcWithPackages;
  };

  hlint = null;
  hscolour = null;
  cabal-macosx = null;

  # doctest doesn't work on ghcjs, but sometimes dontCheck doesn't seem to get rid of the dependency
  doctest = builtins.trace "Warning: ignoring dependency on doctest" null;

  # These packages require doctest
  comonad = dontCheck super.comonad;
  http-types = dontCheck super.http-types;
  lens = disableCabalFlag (disableCabalFlag (dontCheck super.lens) "test-properties") "test-doctests";
  semigroupoids = disableCabalFlag super.semigroupoids "doctests";
  these = dontCheck super.these;

  # Need newer version of colour for some reason.
  colour = dontCheck (super.colour.overrideAttrs (drv: {
    src = nixpkgs.fetchurl {
      url = "http://hackage.haskell.org/package/colour-2.3.4/colour-2.3.4.tar.gz";
      sha256 = "1sy51nz096sv91nxqk6yk7b92b5a40axv9183xakvki2nc09yhqg";
    };
  }));
}
