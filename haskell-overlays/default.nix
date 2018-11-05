{ lib
, haskellLib
, nixpkgs, fetchFromGitHub, hackGet
, ghcjsBaseSrc, ghcjsBaseTextJSStringSrc
, useFastWeak, useReflexOptimizer, enableLibraryProfiling, enableTraceReflexEvents
, useTextJSString, enableExposeAllUnfoldings
, stage2Script
, optionalExtension
, androidActivity
, ghcSavedSplices
, haskellOverlays
}:

rec {
  versionWildcard = versionList: let
    versionListInc = lib.init versionList ++ [ (lib.last versionList + 1) ];
    bottom = lib.concatStringsSep "." (map toString versionList);
    top = lib.concatStringsSep "." (map toString versionListInc);
  in version: lib.versionOlder version top && lib.versionAtLeast version bottom;

  foldExtensions = lib.foldr lib.composeExtensions (_: _: {});

  getGhcVersion = ghc:
    if ghc.isGhcjs or false
    then ghc.ghcVersion
    else ghc.version;

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

    combined-any
    (optionalExtension (!(super.ghc.isGhcjs or false)) combined-ghc)
    (optionalExtension (super.ghc.isGhcjs or false) combined-ghcjs)

    (optionalExtension (super.ghc.isGhcjs or false && useTextJSString) textJSString)
    (optionalExtension (with nixpkgs.stdenv; versionWildcard [ 8 2 ] super.ghc.version && hostPlatform != buildPlatform) disableTemplateHaskell)
    (optionalExtension (with nixpkgs.stdenv; versionWildcard [ 8 4 ] super.ghc.version && hostPlatform != buildPlatform) loadSplices)

    (optionalExtension (nixpkgs.stdenv.hostPlatform.useAndroidPrebuilt or false) android)
    (optionalExtension (nixpkgs.stdenv.hostPlatform.isiOS or false) ios)

    user-custom
  ] self super;

  combined-any = self: super: foldExtensions [
    any
    (optionalExtension (versionWildcard [ 7 ] (getGhcVersion super.ghc)) combined-any-7)
    (optionalExtension (versionWildcard [ 8 ] (getGhcVersion super.ghc)) combined-any-8)
  ] self super;

  combined-any-7 = self: super: foldExtensions [
    any-7
    (optionalExtension (versionWildcard [ 7 8 ] (getGhcVersion super.ghc)) any-7_8)
  ] self super;

  combined-any-8 = self: super: foldExtensions [
    any-8
    (optionalExtension (versionWildcard [ 8 0 ] (getGhcVersion super.ghc)) any-8_0)
    (optionalExtension (versionWildcard [ 8 2 ] (getGhcVersion super.ghc)) any-8_2)
    (optionalExtension (versionWildcard [ 8 4 ] (getGhcVersion super.ghc)) any-8_4)
    (optionalExtension (lib.versionOlder "8.5"  (getGhcVersion super.ghc)) any-head)
  ] self super;

  combined-ghc = self: super: foldExtensions [
    (optionalExtension (versionWildcard [ 8 0 ] super.ghc.version) ghc-8_0)
    (optionalExtension (versionWildcard [ 8 2 ] super.ghc.version) ghc-8_2)
    (optionalExtension (versionWildcard [ 8 4 ] super.ghc.version) ghc-8_4)
    (optionalExtension (lib.versionOlder "8.5"  super.ghc.version) ghc-head)
  ] self super;

  combined-ghcjs = self: super: foldExtensions [
    ghcjs
    (optionalExtension (versionWildcard [ 8 0 ] super.ghc.ghcVersion) ghcjs-8_0)
    (optionalExtension (versionWildcard [ 8 2 ] super.ghc.ghcVersion) ghcjs-8_2)
    (optionalExtension (versionWildcard [ 8 4 ] super.ghc.ghcVersion) ghcjs-8_4)
  ] self super;

  ##
  ## Constituent
  ##

  reflexPackages = import ./reflex-packages.nix {
    inherit haskellLib lib nixpkgs fetchFromGitHub hackGet useFastWeak useReflexOptimizer enableTraceReflexEvents enableLibraryProfiling;
  };
  disableTemplateHaskell = import ./disable-template-haskell.nix {
    inherit haskellLib fetchFromGitHub;
  };
  exposeAllUnfoldings = import ./expose-all-unfoldings.nix { };
  textJSString = import ./text-jsstring {
    inherit lib haskellLib fetchFromGitHub hackGet ghcjsBaseTextJSStringSrc versionWildcard;
    inherit (nixpkgs) fetchpatch;
  };

  # For GHC and GHCJS
  any = _: _: {};
  any-7 = import ./any-7.nix { inherit haskellLib; };
  any-7_8 = import ./any-7.8.nix { inherit haskellLib; };
  any-8 = import ./any-8.nix { inherit haskellLib lib getGhcVersion; };
  any-8_0 = import ./any-8.0.nix { inherit haskellLib; };
  any-8_2 = import ./any-8.2.nix { inherit haskellLib fetchFromGitHub; };
  any-8_4 = import ./any-8.4.nix { inherit haskellLib fetchFromGitHub; inherit (nixpkgs) pkgs; };
  any-head = import ./any-head.nix { inherit haskellLib fetchFromGitHub; };

  # Just for GHC, usually to sync with GHCJS
  ghc-8_0 = import ./ghc-8.0.nix { inherit haskellLib stage2Script; };
  ghc-8_2 = _: _: {};
  ghc-8_4 = _: _: {};
  ghc-head = _: _: {};

  saveSplices = import ./save-splices.nix {
    inherit lib haskellLib fetchFromGitHub;
  };

  loadSplices = import ./load-splices.nix {
    inherit lib haskellLib fetchFromGitHub;
    splicedHaskellPackages = ghcSavedSplices;
  };

  # Just for GHCJS
  ghcjs = import ./ghcjs.nix {
    inherit haskellLib nixpkgs fetchFromGitHub ghcjsBaseSrc useReflexOptimizer hackGet;
  };
  ghcjs-8_0 = self: super: {
    hashable = haskellLib.addBuildDepend (self.callHackage "hashable" "1.2.7.0" {}) self.text;
    # `configure` cannot be generated on the fly from `configure.ac` with older Cabal.
    old-time = haskellLib.addBuildTool super.old-time nixpkgs.autoreconfHook;
  };
  ghcjs-8_2 = _: _: {
  };
  ghcjs-8_4 = optionalExtension useTextJSString (_: _: {
    dlist = null;
    ghcjs-base = null;
    primitive = null;
    vector = null;
  });

  android = import ./android {
    inherit haskellLib;
    inherit nixpkgs;
    inherit androidActivity;
  };
  ios = import ./ios.nix {
    inherit haskellLib;
    inherit (nixpkgs) lib;
  };

  untriaged = import ./untriaged.nix {
    inherit haskellLib;
    inherit fetchFromGitHub;
    inherit enableLibraryProfiling;
  };

  user-custom = foldExtensions haskellOverlays;
}
