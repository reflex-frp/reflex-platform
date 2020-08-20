{ haskellLib
, lib, nixpkgs
, thunkSet, fetchFromGitHub, fetchFromBitbucket, hackGet
, useFastWeak, useReflexOptimizer, enableTraceReflexEvents, enableLibraryProfiling, __useTemplateHaskell
}:

with haskellLib;

self: super:

let
  universeRepo = self._dep.universe;
  reflexDomRepo = self._dep.reflex-dom;
  jsaddleSrc = self._dep.jsaddle;
  gargoylePkgs = self.callPackage self._dep.gargoyle self;
  wasmCross = hackGet ../../wasm-cross;

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
  in haskellLib.dontCheck (haskellLib.overrideCabal
    (self.callCabal2nixWithOptions "reflex-dom-core" (reflexDomRepo + "/reflex-dom-core") (lib.concatStringsSep " " (lib.concatLists [
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
    }));

  reflex-dom = haskellLib.overrideCabal
    (self.callCabal2nixWithOptions "reflex-dom" (reflexDomRepo + "/reflex-dom") (lib.concatStringsSep " " (lib.concatLists [
      reflexOptimizerFlag
      useTemplateHaskellFlag
    ])) {})
    (drv: {
      # Hack until https://github.com/NixOS/cabal2nix/pull/432 lands
      libraryHaskellDepends = (drv.libraryHaskellDepends or [])
        ++ stdenv.lib.optionals (with stdenv.hostPlatform; isAndroid && is32bit) [
        self.android-activity
      ] ++ stdenv.lib.optionals (with stdenv.hostPlatform; isWasm && is32bit) [
        self.jsaddle-wasm
      ];
    });

  chrome-test-utils = self.callCabal2nix "chrome-test-utils" (reflexDomRepo + "/chrome-test-utils") {};

  ##
  ## Terminal / Conventional OS
  ##

  reflex-vty = self.callHackage "reflex-vty" "0.1.4.0" {};
  reflex-process = self.callHackage "reflex-process" "0.3.0.0" {};
  reflex-fsnotify = self.callHackage "reflex-fsnotify" "0.2.1.1" {};

  ##
  ## Tooling
  ##

  reflex-ghci = self.callCabal2nix "reflex-ghci" self._dep.reflex-ghci {};

  ##
  ## GHCJS and JSaddle
  ##

  jsaddle = self.callCabal2nix "jsaddle" (jsaddleSrc + "/jsaddle") {};
  jsaddle-clib = self.callCabal2nix "jsaddle-clib" (jsaddleSrc + "/jsaddle-clib") {};
  jsaddle-webkit2gtk = self.callCabal2nix "jsaddle-webkit2gtk" (jsaddleSrc + "/jsaddle-webkit2gtk") {};
  jsaddle-webkitgtk = self.callCabal2nix "jsaddle-webkitgtk" (jsaddleSrc + "/jsaddle-webkitgtk") {};
  jsaddle-wkwebview = overrideCabal (self.callCabal2nix "jsaddle-wkwebview" (jsaddleSrc + "/jsaddle-wkwebview") {}) (drv: {
    libraryFrameworkDepends = (drv.libraryFrameworkDepends or []) ++
      (if nixpkgs.stdenv.hostPlatform.useiOSPrebuilt then [
         "${nixpkgs.buildPackages.darwin.xcode}/Contents/Developer/Platforms/${nixpkgs.stdenv.hostPlatform.xcodePlatform}.platform/Developer/SDKs/${nixpkgs.stdenv.hostPlatform.xcodePlatform}.sdk/System"
       ] else (with nixpkgs.buildPackages.darwin.apple_sdk.frameworks; [ Cocoa WebKit  ]));
    buildDepends = lib.optional (!nixpkgs.stdenv.hostPlatform.useiOSPrebuilt) [ nixpkgs.buildPackages.darwin.cf-private ];
  });

  # another broken test
  # phantomjs has issues with finding the right port
  # jsaddle-warp = dontCheck (addTestToolDepend (self.callCabal2nix "jsaddle-warp" "${jsaddleSrc}/jsaddle-warp" {}));
  jsaddle-warp = dontCheck (self.callCabal2nix "jsaddle-warp" (jsaddleSrc + "/jsaddle-warp") {});

  jsaddle-dom = dontCheck (appendPatch (self.callCabal2nix "jsaddle-dom" self._dep.jsaddle-dom {}) ./fix-jsaddle-dom.patch);
  #jsaddle-dom = self.callCabal2nix "jsaddle-dom" self._dep.jsaddle-dom {};
  jsaddle-wasm = self.callCabal2nix "jsaddle-wasm" (hackGet (wasmCross + "/jsaddle-wasm")) {};
  ghcjs-dom = self.callCabal2nix "ghcjs-dom" (self._dep.ghcjs-dom + "/ghcjs-dom") {};
  ghcjs-dom-jsaddle = self.callCabal2nix "ghcjs-dom-jsaddle" (self._dep.ghcjs-dom + "/ghcjs-dom-jsaddle") {};
  ghcjs-dom-jsffi = self.callCabal2nix "ghcjs-dom-jsffi" (self._dep.ghcjs-dom + "/ghcjs-dom-jsffi") {};

  ##
  ## Gargoyle
  ##

  inherit (gargoylePkgs) gargoyle gargoyle-postgresql gargoyle-postgresql-nix;

  ##
  ## Misc other dependencies
  ##

  haskell-gi-overloading = dontHaddock (self.callHackage "haskell-gi-overloading" "0.0" {});

  monoidal-containers = self.callHackage "monoidal-containers" "0.6.0.1" {};

  patch = self.callCabal2nix "patch" self._dep.patch {};

  # Not on Hackage yet
  # Version 1.2.1 not on Hackage yet
  hspec-webdriver = self.callCabal2nix "hspec-webdriver" (fetchFromBitbucket {
    owner = "wuzzeb";
    repo = "webdriver-utils";
    rev = "a8b15525a1cceb0ddc47cfd4d7ab5a29fdbe3127";
    sha256 = "0csmxyxkxqgx0v2vwphz80515nqz1hpw5v7391fqpjm7bfgy47k4";
  } + "/hspec-webdriver") {};

  constraints-extras = self.callHackage "constraints-extras" "0.3.0.1" {};
  dependent-map = self.callHackage "dependent-map" "0.3" {};
  dependent-sum = self.callHackage "dependent-sum" "0.6.2.0" {};
  dependent-sum-template = self.callHackage "dependent-sum-template" "0.1.0.0" {};
  dependent-sum-universe-orphans = self.callCabal2nix "dependent-sum-universe-orphans" self._dep.dependent-sum-universe-orphans {};

  # Need to use `--subpath` because LICENSE in each dir is a symlink to the repo root.
  universe = self.callCabal2nixWithOptions "universe" universeRepo "--subpath universe" {};
  universe-base = self.callCabal2nixWithOptions "universe" universeRepo "--subpath universe-base" {};
  universe-dependent-sum = nixpkgs.haskell.lib.doJailbreak (self.callCabal2nixWithOptions "universe" universeRepo "--subpath universe-dependent-sum" {});
  universe-instances-extended = self.callCabal2nixWithOptions "universe" universeRepo "--subpath universe-instances-extended" {};
  universe-reverse-instances = self.callCabal2nixWithOptions "universe" universeRepo "--subpath universe-reverse-instances" {};
  universe-instances-base = self.callCabal2nixWithOptions "universe" universeRepo "--subpath deprecated/universe-instances-base" {};

  # Needed to fix cross compilation from macOS to elsewhere
  # https://github.com/danfran/cabal-macosx/pull/14
  cabal-macosx = self.callCabal2nix "cabal-macosx" self._dep.cabal-macosx {};
}
