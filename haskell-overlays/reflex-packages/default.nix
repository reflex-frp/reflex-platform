{ haskellLib
, lib, nixpkgs
, thunkSet, fetchFromGitHub, fetchFromBitbucket
, useFastWeak, useReflexOptimizer, enableTraceReflexEvents, enableLibraryProfiling
}:

with haskellLib;

self: super:

let
  universeRepo = nixpkgs.hackGet ./dep/universe;
  reflexDom = import self._dep.reflex-dom self;
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
  hackGet = nixpkgs.hackGet;
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

  jsaddle = self.callCabal2nix "jsaddle" (jsaddleSrc + /jsaddle) {};
  jsaddle-clib = self.callCabal2nix "jsaddle-clib" (jsaddleSrc + /jsaddle-clib) {};
  jsaddle-webkit2gtk = self.callCabal2nix "jsaddle-webkit2gtk" (jsaddleSrc + /jsaddle-webkit2gtk) {};
  jsaddle-webkitgtk = self.callCabal2nix "jsaddle-webkitgtk" (jsaddleSrc + /jsaddle-webkitgtk) {};
  jsaddle-wkwebview = overrideCabal (self.callCabal2nix "jsaddle-wkwebview" (jsaddleSrc + /jsaddle-wkwebview) {}) (drv: {
    libraryFrameworkDepends = (drv.libraryFrameworkDepends or []) ++
      (if nixpkgs.stdenv.hostPlatform.useiOSPrebuilt then [
         "${nixpkgs.buildPackages.darwin.xcode}/Contents/Developer/Platforms/${nixpkgs.stdenv.hostPlatform.xcodePlatform}.platform/Developer/SDKs/${nixpkgs.stdenv.hostPlatform.xcodePlatform}.sdk/System"
       ] else (with nixpkgs.buildPackages.darwin.apple_sdk.frameworks; [ Cocoa WebKit  ]));
    buildDepends = lib.optional (!nixpkgs.stdenv.hostPlatform.useiOSPrebuilt) [ nixpkgs.buildPackages.darwin.cf-private ];
  });

  # another broken test
  # phantomjs has issues with finding the right port
  # jsaddle-warp = dontCheck (addTestToolDepend (self.callCabal2nix "jsaddle-warp" "${jsaddleSrc}/jsaddle-warp" {}));
  jsaddle-warp = dontCheck (self.callCabal2nix "jsaddle-warp" (jsaddleSrc + /jsaddle-warp) {});

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

  monoidal-containers = self.callHackageDirect {
    pkg = "monoidal-containers";
    ver = "0.6";
    sha256 = "0vc889wlxs1r99k3615yk30d935jhn45rc8sc6bayi83lyb9a8cj";
  } {};

  # Not on Hackage yet
  # Version 1.2.1 not on Hackage yet
  hspec-webdriver = self.callCabal2nix "hspec-webdriver" (fetchFromBitbucket {
    owner = "wuzzeb";
    repo = "webdriver-utils";
    rev = "a8b15525a1cceb0ddc47cfd4d7ab5a29fdbe3127";
    sha256 = "0csmxyxkxqgx0v2vwphz80515nqz1hpw5v7391fqpjm7bfgy47k4";
  } + "/hspec-webdriver") {};

  constraints-extras = self.callHackage "constraints-extras" "0.3.0.1" {};
  dependent-map = self.callHackageDirect {
    pkg = "dependent-map";
    ver = "0.3";
    sha256 = "1r4n7ivbkrrm6h8s124gj23bjv2kcx5sb4bfp1hriqsng3fgkifi";
  } {};
  dependent-sum = self.callHackageDirect {
    pkg = "dependent-sum";
    ver = "0.6.2.0";
    sha256 = "12k9wfl0i7g5mp9klh2720wz8rqxz4jl63zjzir9nxycb90qkxd5";
  } {};
  dependent-sum-template = self.callHackageDirect {
    pkg = "dependent-sum-template";
    ver = "0.1.0.0";
    sha256 = "0fm73cbja570lfxznv66daya5anp4b0m24jjm5fwn95f49dp9d4n";
  } {};
  dependent-sum-universe-orphans = self.callCabal2nix "dependent-sum-universe-orphans" (hackGet ./dep/dependent-sum-universe-orphans) {};

  universe = self.callCabal2nixWithOptions "universe" universeRepo "--subpath universe" {};
  universe-base = self.callCabal2nixWithOptions "universe" universeRepo "--subpath universe-base" {};
  universe-dependent-sum = nixpkgs.haskell.lib.doJailbreak (self.callCabal2nixWithOptions "universe" universeRepo "--subpath universe-dependent-sum" {});
  universe-instances-extended = self.callCabal2nixWithOptions "universe" universeRepo "--subpath universe-instances-extended" {};
  universe-reverse-instances = self.callCabal2nixWithOptions "universe" universeRepo "--subpath universe-reverse-instances" {};
  universe-instances-base = self.callCabal2nixWithOptions "universe" universeRepo "--subpath deprecated/universe-instances-base" {};

}
