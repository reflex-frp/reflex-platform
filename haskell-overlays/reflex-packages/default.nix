{ haskellLib
, lib, nixpkgs
, thunkSet, fetchFromGitHub, fetchFromBitbucket
, useFastWeak, useReflexOptimizer, enableTraceReflexEvents, enableLibraryProfiling, __useTemplateHaskell
}:

with haskellLib;

self: super:

let
  universeRepo = self._dep.universe;
  reflexDomRepo = self._dep.reflex-dom;
  jsaddleSrc = self._dep.jsaddle;
  gargoylePkgs = self.callPackage self._dep.gargoyle self;
  ghcjsDom = import self._dep.ghcjs-dom self;

  reflexOptimizerFlag = lib.optional (useReflexOptimizer && (self.ghc.cross or null) == null) "-fuse-reflex-optimizer";
  useTemplateHaskellFlag = lib.optional (!__useTemplateHaskell) "-f-use-template-haskell";

  inherit (nixpkgs) stdenv;
in
{
  _dep = super._dep or {} // thunkSet ./dep;

  ##
  ## Reflex family
  ##

  reflex = self.callCabal2nixWithOptions "reflex" self._dep.reflex (lib.concatStringsSep " " (lib.concatLists [
    (lib.optional enableTraceReflexEvents "-fdebug-trace-events")
    reflexOptimizerFlag
    useTemplateHaskellFlag
    (lib.optional useFastWeak "-ffast-weak")
  ])) {};

  reflex-todomvc = self.callPackage self._dep.reflex-todomvc {};
  reflex-aeson-orphans = self.callCabal2nix "reflex-aeson-orphans" self._dep.reflex-aeson-orphans {};

  reflex-dom-core = let
    inherit (self) ghc;
    noGcTest = stdenv.hostPlatform.system != "x86_64-linux"
            || stdenv.hostPlatform != stdenv.buildPlatform
            || (ghc.isGhcjs or false);
  in haskellLib.overrideCabal
    (self.callCabal2nixWithOptions "reflex-dom-core" reflexDomRepo (lib.concatStringsSep " " (lib.concatLists [
      ["--subpath reflex-dom-core"]
      reflexOptimizerFlag
      useTemplateHaskellFlag
      (lib.optional enableLibraryProfiling "-fprofile-reflex")
    ])) {})
    (drv: {
      # TODO: Get hlint working for cross-compilation
      doCheck = stdenv.hostPlatform == stdenv.buildPlatform && !(ghc.isGhcjs or false);

      # The headless browser run as part of the tests will exit without this
      preBuild = ''
        export HOME="$PWD"
      '';

      # Show some output while running tests, so we might notice what's wrong
      testTarget = "--show-details=streaming";

      testHaskellDepends = with self; (drv.testHaskellDepends or []) ++ stdenv.lib.optionals (!noGcTest) [
        temporary
        jsaddle-warp
        process
        chrome-test-utils
      ];

      testSystemDepends = with nixpkgs; (drv.testSystemDepends or []) ++ [
        selenium-server-standalone which
      ] ++ stdenv.lib.optionals (!noGcTest) [
        chromium
        nixpkgs.iproute
      ];
    } // stdenv.lib.optionalAttrs (!noGcTest) {
      # The headless browser run as part of gc tests would hang/crash without this
      preCheck = ''
        export FONTCONFIG_PATH=${nixpkgs.fontconfig.out}/etc/fonts
      '';
    });

  reflex-dom = haskellLib.overrideCabal
    (self.callCabal2nixWithOptions "reflex-dom" reflexDomRepo (lib.concatStringsSep " " (lib.concatLists [
      ["--subpath reflex-dom"]
      reflexOptimizerFlag
      useTemplateHaskellFlag
    ])) {})
    (drv: {
      # Hack until https://github.com/NixOS/cabal2nix/pull/432 lands
      libraryHaskellDepends = (drv.libraryHaskellDepends or []) ++ stdenv.lib.optionals (with stdenv.hostPlatform; isAndroid && is32bit) [
        self.android-activity
      ];
    });

  chrome-test-utils = self.callCabal2nixWithOptions "chrome-test-utils" reflexDomRepo "--subpath chrome-test-utils" {};

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
  dependent-sum-universe-orphans = self.callCabal2nix "dependent-sum-universe-orphans" self._dep.dependent-sum-universe-orphans {};

  universe = self.callCabal2nixWithOptions "universe" universeRepo "--subpath universe" {};
  universe-base = self.callCabal2nixWithOptions "universe" universeRepo "--subpath universe-base" {};
  universe-dependent-sum = nixpkgs.haskell.lib.doJailbreak (self.callCabal2nixWithOptions "universe" universeRepo "--subpath universe-dependent-sum" {});
  universe-instances-extended = self.callCabal2nixWithOptions "universe" universeRepo "--subpath universe-instances-extended" {};
  universe-reverse-instances = self.callCabal2nixWithOptions "universe" universeRepo "--subpath universe-reverse-instances" {};
  universe-instances-base = self.callCabal2nixWithOptions "universe" universeRepo "--subpath deprecated/universe-instances-base" {};

}
