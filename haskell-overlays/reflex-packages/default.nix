{ haskellLib
, lib, nixpkgs
, thunkSet, fetchFromGitHub, fetchFromBitbucket
, useFastWeak, useReflexOptimizer, enableTraceReflexEvents, enableLibraryProfiling
}:

with haskellLib;

self: super:

let
  reflexDom = import self._dep.reflex-dom self nixpkgs;
  jsaddleSrc = self._dep.jsaddle;
  gargoylePkgs = self.callPackage self._dep.gargoyle self;
  ghcjsDom = import self._dep.ghcjs-dom self;
  addReflexTraceEventsFlag = drv: if enableTraceReflexEvents
    then appendConfigureFlag drv "-fdebug-trace-events"
    else drv;
  addReflexOptimizerFlag = drv: if useReflexOptimizer && (self.ghc.cross or null) == null
    then appendConfigureFlag drv "-fuse-reflex-optimizer"
    else drv;
  addFastWeakFlag = drv: if useFastWeak
    then enableCabalFlag drv "fast-weak"
    else drv;
in
{
  _dep = super._dep or {} // thunkSet ./dep;

  ##
  ## Reflex family
  ##

  reflex = dontCheck (addFastWeakFlag (addReflexTraceEventsFlag (addReflexOptimizerFlag (self.callPackage self._dep.reflex {}))));
  reflex-todomvc = self.callPackage self._dep.reflex-todomvc {};
  reflex-aeson-orphans = self.callCabal2nix "reflex-aeson-orphans" self._dep.reflex-aeson-orphans {};
  reflex-dom = addReflexOptimizerFlag reflexDom.reflex-dom;
  reflex-dom-core = appendConfigureFlags
    (addReflexOptimizerFlag reflexDom.reflex-dom-core)
    (lib.optional enableLibraryProfiling "-fprofile-reflex");
  chrome-test-utils = reflexDom.chrome-test-utils;

  ##
  ## GHCJS and JSaddle
  ##

  jsaddle = self.callCabal2nix "jsaddle" "${jsaddleSrc}/jsaddle" {};
  jsaddle-clib = self.callCabal2nix "jsaddle-clib" "${jsaddleSrc}/jsaddle-clib" {};
  jsaddle-webkit2gtk = self.callCabal2nix "jsaddle-webkit2gtk" "${jsaddleSrc}/jsaddle-webkit2gtk" {};
  jsaddle-webkitgtk = self.callCabal2nix "jsaddle-webkitgtk" "${jsaddleSrc}/jsaddle-webkitgtk" {};
  jsaddle-wkwebview = overrideCabal (self.callCabal2nix "jsaddle-wkwebview" "${jsaddleSrc}/jsaddle-wkwebview" {}) (drv: {
    # HACK(matthewbauer): Can’t figure out why cf-private framework is
    #                     not getting pulled in correctly. Has something
    #                     to with how headers are looked up in xcode.
    preBuild = lib.optionalString (!nixpkgs.stdenv.hostPlatform.useiOSPrebuilt) ''
      mkdir include
      ln -s ${nixpkgs.buildPackages.darwin.cf-private}/Library/Frameworks/CoreFoundation.framework/Headers include/CoreFoundation
      export NIX_CFLAGS_COMPILE="-I$PWD/include $NIX_CFLAGS_COMPILE"
    '';

    libraryFrameworkDepends = (drv.libraryFrameworkDepends or []) ++
      (if nixpkgs.stdenv.hostPlatform.useiOSPrebuilt then [
         "${nixpkgs.buildPackages.darwin.xcode}/Contents/Developer/Platforms/${nixpkgs.stdenv.hostPlatform.xcodePlatform}.platform/Developer/SDKs/${nixpkgs.stdenv.hostPlatform.xcodePlatform}.sdk/System"
       ] else with nixpkgs.buildPackages.darwin; with apple_sdk.frameworks; [
         Cocoa
         WebKit
       ]);
  });

  # another broken test
  # phantomjs has issues with finding the right port
  # jsaddle-warp = dontCheck (addTestToolDepend (self.callCabal2nix "jsaddle-warp" "${jsaddleSrc}/jsaddle-warp" {}));
  jsaddle-warp = dontCheck (self.callCabal2nix "jsaddle-warp" "${jsaddleSrc}/jsaddle-warp" {});

  jsaddle-dom = self.callPackage self._dep.jsaddle-dom {};
  inherit (ghcjsDom) ghcjs-dom-jsffi;

  ##
  ## Gargoyle
  ##

  inherit (gargoylePkgs) gargoyle gargoyle-postgresql gargoyle-postgresql-nix;

  ##
  ## Misc other dependencies
  ##

  haskell-gi-overloading = dontHaddock (self.callHackage "haskell-gi-overloading" "0.0" {});

  monoidal-containers = self.callHackage "monoidal-containers" "0.4.0.0" {};

  # Need fork
  constraints-extras = self.callCabal2nix "constraints-extras" (fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "constraints-extras";
    rev = "30f10c03dd96e50c089f0613f99951805bff7397";
    sha256 = "196b8kbcp744gqhh964m54vw4cdg15p6lc7cm2vxbh15cbqdz7ir";
  }) {};
  # Need new one
  dependent-map = self.callCabal2nix "dependent-map" (fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "dependent-map";
    rev = "a9a438b1fd974891e4ed57cbd43e305cf7c759a9";
    sha256 = "1g5zyz8rna57g585xrix1ica33865dw7x1kmxwcdh21pp1mqxzn2";
  }) {};
  # Needs additional instances
  dependent-sum = self.callCabal2nixWithOptions "dependent-sum" (fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "dependent-sum";
    rev = "69127f57533e7e58c54c2f9ca19b0b45946d6cc7";
    sha256 = "112cknyz9dl3xbzx715bkhcy0l5z0v98rf06xf70b0fzsgfw68ws";
  }) "--subpath dependent-sum" {};
  # Misc new features since Hackage relasese
  dependent-sum-template = self.callCabal2nixWithOptions "dependent-sum-template" (fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "dependent-sum";
    rev = "69127f57533e7e58c54c2f9ca19b0b45946d6cc7";
    sha256 = "112cknyz9dl3xbzx715bkhcy0l5z0v98rf06xf70b0fzsgfw68ws";
  }) "--subpath dependent-sum-template" {};
  # Not on Hackage yet
  dependent-sum-universe-orphans = self.callCabal2nix "dependent-sum-universe-orphans" (fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "dependent-sum-universe-orphans";
    rev = "8c28c09991cd7c3588ae6db1be59a0540758f5f5";
    sha256 = "0dg32s2mgxav68yw6g7b15w0h0z116zx0qri26gprafgy23bxanm";
  }) {};
  # Version 1.2.1 not on Hackage yet
  hspec-webdriver = self.callCabal2nix "hspec-webdriver" (fetchFromBitbucket {
    owner = "wuzzeb";
    repo = "webdriver-utils";
    rev = "a8b15525a1cceb0ddc47cfd4d7ab5a29fdbe3127";
    sha256 = "0csmxyxkxqgx0v2vwphz80515nqz1hpw5v7391fqpjm7bfgy47k4";
  } + "/hspec-webdriver") {};

}
