{ system, lib, haskellLib, nixpkgs, fetchgit, fetchFromGitHub, useReflexOptimizer }:

with haskellLib;

self: super:

# Workaround for https://github.com/ghcjs/ghcjs/issues/674
# 'os (osx)' will always evaluate to 'false'
let dontHardcodeLinux = package: cabalFile:
  if ! self.ghc.isGhcjs then package
  else nixpkgs.haskell.lib.overrideCabal package (drv: {
    postPatch = (drv.postPatch or "") + nixpkgs.lib.optionalString (system == "x86_64-darwin") ''
      substituteInPlace ${cabalFile}.cabal --replace 'if os(linux)' 'if os(linux) && !impl(ghcjs)'
      substituteInPlace ${cabalFile}.cabal --replace 'if os(osx)' 'if os(linux) && impl(ghcjs)'
    '';
  });
in {
  _dep = super._dep or {} // {
    ghcjsBaseSrc = fetchgit {
      url = "https://github.com/ghcjs/ghcjs-base.git";
      rev = "01014ade3f8f5ae677df192d7c2a208bd795b96c";
      sha256 = "173h98m7namxj0kfy8fj29qcxmcz6ilg04x8mwkc3ydjqrvk77hh";
      postFetch = ''
        ( cd $out
          patch -p1 < ${nixpkgs.fetchpatch {
            url = "https://github.com/ghcjs/ghcjs-base/commit/2d0d674e54c273ed5fcb9a13f588819c3303a865.patch"; #ghcjs-base/114
            sha256 = "15vbxnxa1fpdcmmx5zx1z92bzsxyb0cbs3hs3g7fb1rkds5qbvgp";
          }}
          patch -p1 < ${nixpkgs.fetchpatch {
            url = "https://github.com/ghcjs/ghcjs-base/commit/8eccb8d937041ba323d62dea6fe8eb1b04b3cc47.patch"; #ghcjs-base/116
            sha256 = "1lqjpg46ydpm856wcq1g7c97d69qcnnqs5jxp2b788z9cfd5n64c";
          }}
          patch -p1 < ${nixpkgs.fetchpatch {
            url = "https://github.com/ghcjs/ghcjs-base/commit/ce91c525b5d4377ba4aefd0d8072dc1659f75ef1.patch"; #ghcjs-base/118
            sha256 = "0f6qca1i60cjzpbq4bc74baa7xrf417cja8nmhfims1fflvsx3wy";
          }}
          patch -p1 < ${nixpkgs.fetchpatch {
            url = "https://github.com/ghcjs/ghcjs-base/commit/213bfc74a051242668edf0533e11a3fafbbb1bfe.patch"; #ghcjs-base/120
            sha256 = "0d5dwy22hxa79l8b4y6nn53nbcs74686s0rmfr5l63sdvqxhdy3x";
          }}
          patch -p1 < ${nixpkgs.fetchpatch {
            url = "https://github.com/ghcjs/ghcjs-base/commit/82d76814ab40dc9116990f69f16df330462f27d4.patch"; #ghcjs-base/121
            sha256 = "0qa74h6w8770csad0bky4hhss1b1s86i6ccpd3ky4ljx00272gqh";
          }}
          patch -p1 < ${nixpkgs.fetchpatch {
            url = "https://github.com/ghcjs/ghcjs-base/commit/5eb34b3dfc6fc9196931178a7a6e2c8a331a8e53.patch"; #ghcjs-base/122
            sha256 = "1wrfi0rscy8qa9pi4siv54pq5alplmy56ym1fbs8n93xwlqhddii";
          }}
          patch -p1 < ${nixpkgs.fetchpatch {
            url = "https://github.com/ghcjs/ghcjs-base/commit/0cf64df77cdd6275d86ec6276fcf947fa58e548b.patch"; #ghcjs-base/122
            sha256 = "16wdghfsrzrb1y7lscbf9aawgxi3kvbgdjwvl1ga2zzm4mq139dr";
          }}
          cat ghcjs-base.cabal
        )
      '';
    };
  };

  ghcWithPackages = selectFrom: self.callPackage (nixpkgs.path + "/pkgs/development/haskell-modules/with-packages-wrapper.nix") {
    inherit (self) llvmPackages;
    packages = selectFrom self;
  } // nixpkgs.lib.optionalAttrs useReflexOptimizer {
    ghcLibdir = "${self.ghc.bootPackages.ghcWithPackages (p: [ p.reflex ])}/lib/${self.ghc.bootPackages.ghc.name}";
  };

  ghcjs-base = doJailbreak (dontCheck (self.callCabal2nix "ghcjs-base" self._dep.ghcjsBaseSrc {}));

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

  foundation = dontHardcodeLinux super.foundation "foundation";
}
