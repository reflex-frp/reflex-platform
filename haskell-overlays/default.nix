{ lib
, haskellLib
, nixpkgs, fetchFromGitHub, hackGet
, useFastWeak, useReflexOptimizer, enableLibraryProfiling, enableTraceReflexEvents
, useTextJSString, enableExposeAllUnfoldings
, stage2Script
, optionalExtension
, androidActivity
}:

rec {
  versionWildcard = versionList: let
    versionListInc = lib.init versionList ++ [ (lib.last versionList + 1) ];
    bottom = lib.concatStringsSep "." (map toString versionList);
    top = lib.concatStringsSep "." (map toString versionListInc);
  in version: lib.versionOlder version top && lib.versionAtLeast version bottom;

  foldExtensions = lib.foldr lib.composeExtensions (_: _: {});


  ##
  ## Conventional roll ups of all the constituent overlays below.
  ##

  # `super.ghc` is used so that the use of an overlay does not depend on that
  # overlay. At the cost of violating the usual rules on using `self` vs
  # `super`, this avoids a bunch of strictness issues keeping us terminating.
  combined = self: super: foldExtensions [
    reflexPackages
    untriaged

    (optionalExtension enableExposeAllUnfoldings exposeAllUnfoldings)

    (optionalExtension (!(super.ghc.isGhcjs or false)) combined-ghc)
    (optionalExtension (super.ghc.isGhcjs or false) ghcjs)

    (optionalExtension (super.ghc.isGhcjs or false && useTextJSString) textJSString)
    (optionalExtension (with nixpkgs.stdenv; hostPlatform != buildPlatform) disableTemplateHaskell)

    (optionalExtension (nixpkgs.stdenv.hostPlatform.useAndroidPrebuilt or false) android)
    (optionalExtension (nixpkgs.stdenv.hostPlatform.useIosPrebuilt or false) ios)
  ] self super;

  combined-ghc = self: super: foldExtensions [
    ghc
    (optionalExtension (versionWildcard [ 7 ] super.ghc.version) combined-ghc-7)
    (optionalExtension (versionWildcard [ 8 ] super.ghc.version) combined-ghc-8)
  ] self super;

  combined-ghc-7 = self: super: foldExtensions [
    ghc-7
    (optionalExtension (versionWildcard [ 7 8 ] super.ghc.version) ghc-7_8)
  ] self super;

  combined-ghc-8 = self: super: foldExtensions [
    ghc-8
    (optionalExtension (versionWildcard [ 8 2 ] super.ghc.version) ghc-8_2)
  ] self super;


  ##
  ## Constituent
  ##

  reflexPackages = import ./reflex-packages.nix {
    inherit haskellLib nixpkgs fetchFromGitHub hackGet useFastWeak useReflexOptimizer enableTraceReflexEvents;
  };
  disableTemplateHaskell = import ./disable-template-haskell.nix {
    inherit haskellLib fetchFromGitHub;
  };
  exposeAllUnfoldings = import ./expose-all-unfoldings.nix { };
  textJSString = import ./text-jsstring {
    inherit haskellLib fetchFromGitHub;
  };

  ghc = import ./ghc.nix { inherit haskellLib stage2Script; };
  ghc-7 = import ./ghc-7.x.y.nix { inherit haskellLib; };
  ghc-7_8 = import ./ghc-7.8.y.nix { inherit haskellLib; };
  ghc-8 = import ./ghc-8.x.y.nix { };
  ghc-8_2 = import ./ghc-8.2.x.nix { inherit haskellLib nixpkgs fetchFromGitHub; };
  ghc-head = import ./ghc-head.nix { inherit haskellLib fetchFromGitHub; };

  ghcjs = import ./ghcjs.nix {
    inherit haskellLib nixpkgs fetchFromGitHub useReflexOptimizer;
  };

  android = import ./android {
    inherit haskellLib;
    inherit androidActivity;
    inherit nixpkgs;
  };
  ios = import ./ios.nix { inherit haskellLib; };

  untriaged = import ./untriaged.nix {
    inherit haskellLib;
    inherit lib;
    inherit nixpkgs;
    inherit fetchFromGitHub;
    inherit enableLibraryProfiling;
  };
}
