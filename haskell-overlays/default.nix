{ haskellLib
, nixpkgs, jdk, fetchFromGitHub
, useReflexOptimizer, stage2Script
, androidActivity
}:

rec {
  disableTemplateHaskell = import ./disable-template-haskell.nix {
    inherit haskellLib fetchFromGitHub;
  };
  exposeAllUnfoldings = import ./expose-all-unfoldings.nix { };
  textJSString = import ./text-jsstring {
    inherit haskellLib fetchFromGitHub;
  };

  ghc = import ./ghc.nix { inherit haskellLib stage2Script; };
  ghc-7 = nixpkgs.lib.composeExtensions
    ghc
    (import ./ghc-7.x.y.nix { inherit haskellLib; });
  ghc-7_8 = nixpkgs.lib.composeExtensions
    ghc-7
    (import ./ghc-7.8.y.nix { inherit haskellLib; });
  ghc-8 = nixpkgs.lib.composeExtensions
    ghc
    (import ./ghc-8.x.y.nix { });
  ghc-8_2_1 = nixpkgs.lib.composeExtensions
    ghc-8
    (import ./ghc-8.2.1.nix { inherit haskellLib fetchFromGitHub; });
  ghc-head = nixpkgs.lib.composeExtensions
    ghc-8
    (import ./ghc-head.nix { inherit haskellLib fetchFromGitHub; });

  ghcjs = import ./ghcjs.nix {
    inherit haskellLib nixpkgs fetchFromGitHub useReflexOptimizer;
  };
  android = import ./android {
    inherit haskellLib;
    inherit androidActivity;
    inherit (nixpkgs) jdk;
  };
  ios = import ./ios.nix { inherit haskellLib; };
}
