{ lib
, haskellLib
, nixpkgs, jdk, fetchFromGitHub
, useReflexOptimizer
, androidActivity
, hackGet
}:

rec {
  exposeAllUnfoldings = import ./expose-all-unfoldings.nix { };
  textJSString = import ./text-jsstring {
    inherit lib haskellLib fetchFromGitHub hackGet;
    inherit (nixpkgs) fetchpatch;
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
  ghc-8_2_2 = nixpkgs.lib.composeExtensions
    ghc-8
    (import ./ghc-8.2.2.nix { inherit haskellLib fetchFromGitHub; });
  ghc-8_4 = nixpkgs.lib.composeExtensions
    ghc-8
    (import ./ghc-8.4.y.nix { inherit haskellLib fetchFromGitHub; inherit (nixpkgs) pkgs; });
  ghc-head = nixpkgs.lib.composeExtensions
    ghc-8
    (import ./ghc-head.nix { inherit haskellLib fetchFromGitHub; });

  ghcjs = import ./ghcjs.nix {
    inherit haskellLib nixpkgs fetchFromGitHub useReflexOptimizer hackGet;
  };
  android = import ./android {
    inherit haskellLib;
    inherit androidActivity;
    inherit (nixpkgs) jdk;
  };
  ios = import ./ios.nix { inherit haskellLib; };
}
