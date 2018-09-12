{ lib
, haskellLib
, nixpkgs, jdk, fetchFromGitHub
, ghcjsBaseSrc
, useReflexOptimizer
, useTextJSString
, optionalExtension
, androidActivity
, hackGet
, ghcSavedSplices
}:

rec {
  exposeAllUnfoldings = import ./expose-all-unfoldings.nix { };
  textJSString = import ./text-jsstring {
    inherit lib haskellLib fetchFromGitHub hackGet;
    inherit (nixpkgs) fetchpatch;
  };

  saveSplices = import ./save-splices.nix {
    inherit lib haskellLib fetchFromGitHub;
  };

  loadSplices = import ./load-splices.nix {
    inherit lib haskellLib fetchFromGitHub;
    splicedHaskellPackages = ghcSavedSplices;
  };

  ghc = import ./ghc.nix { inherit haskellLib; };
  ghc-7 = nixpkgs.lib.composeExtensions
    ghc
    (import ./ghc-7.x.y.nix { inherit haskellLib; });
  ghc-7_8 = nixpkgs.lib.composeExtensions
    ghc-7
    (import ./ghc-7.8.y.nix { inherit haskellLib; });
  ghc-8 = nixpkgs.lib.composeExtensions
    ghc
    (import ./ghc-8.x.y.nix { });
  ghc-8_2 = nixpkgs.lib.composeExtensions
    ghc-8
    (import ./ghc-8.2.2.nix { inherit haskellLib fetchFromGitHub; });
  ghc-8_4 = nixpkgs.lib.composeExtensions
    ghc-8
    (import ./ghc-8.4.y.nix { inherit haskellLib fetchFromGitHub; inherit (nixpkgs) pkgs; });
  ghc-head = nixpkgs.lib.composeExtensions
    ghc-8
    (import ./ghc-head.nix { inherit haskellLib fetchFromGitHub; });

  ghcjs = import ./ghcjs.nix {
    inherit haskellLib nixpkgs fetchFromGitHub ghcjsBaseSrc useReflexOptimizer hackGet;
  };
  ghcjs-8_4 = nixpkgs.lib.composeExtensions
    ghcjs
    (optionalExtension useTextJSString (_: _: {
      dlist = null;
      ghcjs-base = null;
      primitive = null;
      vector = null;
    }));
  android = import ./android {
    inherit haskellLib;
    inherit androidActivity;
    inherit (nixpkgs) jdk lib;
    inherit nixpkgs;
  };
  ios = import ./ios.nix {
    inherit haskellLib;
    inherit (nixpkgs) lib;
  };
}
