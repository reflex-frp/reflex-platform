{ lib, haskellLib, nixpkgs, fetchgit, fetchFromGitHub
, useReflexOptimizer
, useTextJSString
, enableLibraryProfiling
}:

with haskellLib;

self: super: rec {
  # Profiling failures seee https://github.com/ghcjs/ghcjs/issues/759
  optparse-applicative = haskellLib.overrideCabal super.optparse-applicative (drv: {
    broken = drv.broken or false || enableLibraryProfiling;
  });
  th-orphans = haskellLib.overrideCabal super.th-orphans (drv: {
    broken = drv.broken or false || enableLibraryProfiling;
  });

  ghcWithPackages = nixpkgs.buildPackages.callPackage (nixpkgs.path + "/pkgs/development/haskell-modules/with-packages-wrapper.nix") {
    haskellPackages = self;
    hoogleWithPackages = super.hoogleWithPackages;
  } // lib.optionalAttrs (useReflexOptimizer) {
    ghcLibdir = lib.optionalString useReflexOptimizer "${self.ghc.bootPkgs.ghcWithPackages (p: [ p.reflex ])}/lib/${self.ghc.bootPkgs.ghc.name}";
  };

  ghc = if !(lib.versionAtLeast super.ghc.ghcVersion "8.2") then super.ghc else super.ghc.overrideAttrs (_: {
    # TODO: I don't think this is needed except for maybe the fast-weak patch, but doing this to preserve hashes.
    phases = [ "unpackPhase" "patchPhase" "buildPhase" ];
  }) // {
    withPackages = self.ghcWithPackages;
  };

  # doctest doesn't work on ghcjs, but sometimes dontCheck doesn't seem to get rid of the dependency
  doctest = lib.warn "ignoring dependency on doctest" null;

  network = haskellLib.overrideCabal super.network (drv: {
    revision = null;
    editedCabalFile = null;
    patches = (drv.patches or []) ++ [ ./ghcjs-network.patch ];
  });

  attoparsec = self.callHackage "attoparsec" "0.13.2.2" {};

  # These packages require doctest
  comonad = dontCheck super.comonad;
  http-types = dontCheck super.http-types;
  lens = disableCabalFlag (disableCabalFlag (dontCheck super.lens) "test-properties") "test-doctests";
  pgp-wordlist = dontCheck super.pgp-wordlist;
  prettyprinter = dontCheck super.prettyprinter;
  semigroupoids = disableCabalFlag super.semigroupoids "doctests";
  these = dontCheck super.these;
  email-validate = dontCheck super.email-validate;
  OneTuple = overrideCabal super.OneTuple (drv: {
    libraryHaskellDepends = (drv.libraryHaskellDepends or []) ++ [
      self.hashable
    ];
  });

  # These tests are not expected to support ghcjs
  QuickCheck = dontCheck super.QuickCheck;
  temporary = dontCheck super.temporary;

  # Custom docs don't build with ghcjs
  # https://github.com/reflex-frp/reflex-platform/issues/631
  servant = haskellLib.overrideCabal super.servant (drv: {
    postInstall = "";
  });

  # These tests never complete
  tasty-quickcheck = dontCheck super.tasty-quickcheck;

  # Convenience: tests take long to finish
  megaparsec = dontCheck super.megaparsec;

  # Cross fix is working for iOS but not JS for some reason
  cabal-macosx = null;

  # Haddock internal error
  patch = dontHaddock super.patch;
  # When we don't use text-jsstring, we hit cabal version too new issue.
  # NOTE(Dylan Green): We need to have an "updated" version of ghcjs-base, although the patch is still needed
  _ghcjsbase = self.callHackage "ghcjs-base" "0.2.0.3" {};
  ghcjs-base = if useTextJSString then self._ghcjsbase else appendPatch (self._ghcjsbase) ./ghcjs-base-cabal-version.patch;
}
