{ haskellLib
, lib
, nixpkgs
, thunkSet
, fetchFromGitHub
, fetchFromBitbucket
, hackGet
, useFastWeak
, useReflexOptimizer
, enableTraceReflexEvents
, enableLibraryProfiling
, __useTemplateHaskell
}:

with haskellLib;

self: super:

let
  universeRepo = self._dep.universe;
  reflexDomRepo = self._dep.reflex-dom;
  jsaddleSrc = self._dep.jsaddle;
  gargoyleSrc = self._dep.gargoyle;
  wasmCross = hackGet ../../wasm-cross;

  reflexOptimizerFlag = lib.optional (useReflexOptimizer && (self.ghc.cross or null) == null) "-fuse-reflex-optimizer";
  useTemplateHaskellFlag = lib.optional (!__useTemplateHaskell) "-f-use-template-haskell";

  inherit (nixpkgs) stdenv;
  # Older chromium for reflex-dom-core test suite
  nixpkgs_oldChromium = import ../../nixpkgs-old-chromium {
    inherit (nixpkgs.stdenv.buildPlatform) system;
    overlays = [
      (self: super: {
        # Disable tests for p11-kit, a dependency of chromium
        # They fail on non-NixOS systems
        # https://github.com/NixOS/nixpkgs/issues/96715
        p11-kit = super.p11-kit.overrideAttrs (oldAttrs: {
          doCheck = false;
        });
      })
    ];
  };
in
{
  _dep = super._dep or { } // thunkSet ./dep;

  ##
  ## Reflex family
  ##

  reflex = self.callCabal2nixWithOptions "reflex" self._dep.reflex
    (lib.concatStringsSep " " (lib.concatLists [
      (lib.optional enableTraceReflexEvents "-fdebug-trace-events")
      reflexOptimizerFlag
      useTemplateHaskellFlag
      (lib.optional useFastWeak "-ffast-weak")
    ]))
    { };

  reflex-todomvc = haskellLib.doJailbreak (self.callPackage self._dep.reflex-todomvc { });
  reflex-aeson-orphans = self.callCabal2nix "reflex-aeson-orphans" self._dep.reflex-aeson-orphans { };

  # The tests for reflex-dom-core are not deterministic, disable them, and run them manually
  reflex-dom-core =
    let
      inherit (self) ghc;
      noGcTest = stdenv.hostPlatform.system != "x86_64-linux"
        || stdenv.hostPlatform != stdenv.buildPlatform
        || (ghc.isGhcjs or false);
    in
    haskellLib.overrideCabal
      (self.callCabal2nixWithOptions "reflex-dom-core" (reflexDomRepo + "/reflex-dom-core")
        (lib.concatStringsSep " " (lib.concatLists [
          reflexOptimizerFlag
          useTemplateHaskellFlag
          (lib.optional enableLibraryProfiling "-fprofile-reflex")
          [ "-f-hydration-tests" ]
          [ "-f-gc-tests" ]
        ]))
        { })
      (drv: {
        # TODO: Get hlint working for cross-compilation
        #doCheck = stdenv.hostPlatform == stdenv.buildPlatform && !(ghc.isGhcjs or false);
        doCheck = false;
        # The headless browser run as part of the tests will exit without this
        preBuild = (drv.preBuild or "") + ''
          export HOME="$PWD"
        '';

        # Show some output while running tests, so we might notice what's wrong
        testTarget = "--show-details=streaming";

        testHaskellDepends = with self; (drv.testHaskellDepends or [ ]) ++ lib.optionals (!noGcTest) [
          temporary
          jsaddle-warp
          process
          chrome-test-utils
        ];

        testSystemDepends = with nixpkgs; (drv.testSystemDepends or [ ]) ++ lib.optionals (nixpkgs.stdenv.hostPlatform.isLinux) [
          nixpkgs_oldChromium.selenium-server-standalone
          nixpkgs_oldChromium.chromium
          which
        ] ++ lib.optionals (!noGcTest) [
          nixpkgs.iproute
        ];
      } // lib.optionalAttrs (!noGcTest) {
        # The headless browser run as part of gc tests would hang/crash without this
        preCheck = ''
          export FONTCONFIG_PATH=${nixpkgs.fontconfig.out}/etc/fonts
        '' + (drv.preCheck or "");
      });

  reflex-dom = haskellLib.doJailbreak (haskellLib.overrideCabal
    (self.callCabal2nixWithOptions "reflex-dom" (reflexDomRepo + "/reflex-dom")
      (lib.concatStringsSep " " (lib.concatLists [
        reflexOptimizerFlag
        useTemplateHaskellFlag
      ]))
      { })
    (drv: {
      preConfigure = (drv.preConfigure or "") + ''
        sed -i 's|aeson >=1.4 && <1.6|aeson -any|g' *.cabal
      '';
      libraryHaskellDepends = [
        self.reflex
        self.reflex-dom-core
        self.jsaddle-webkit2gtk
        self.aeson
      ] ++ lib.optional (nixpkgs.stdenv.hostPlatform.useAndroidPrebuilt or false) self.android-activity;
    }));

  chrome-test-utils = self.callCabal2nix "chrome-test-utils" (reflexDomRepo + "/chrome-test-utils") { };

  ##
  ## Terminal / Conventional OS
  ##

  reflex-vty = haskellLib.doJailbreak (self.callCabal2nix "reflex-vty" self._dep.reflex-vty { });
  reflex-process = self.callCabal2nix "reflex-process" self._dep.reflex-process { };
  reflex-fsnotify = self.callHackage "reflex-fsnotify" "0.2.1.2" { };

  ##
  ## Tooling
  ##

  reflex-ghci = self.callCabal2nix "reflex-ghci" self._dep.reflex-ghci { };

  ##
  ## GHCJS and JSaddle
  ##

  jsaddle = self.callCabal2nix "jsaddle" (jsaddleSrc + "/jsaddle") { };
  jsaddle-clib = self.callCabal2nix "jsaddle-clib" (jsaddleSrc + "/jsaddle-clib") { };
  jsaddle-webkit2gtk = self.callCabal2nix "jsaddle-webkit2gtk" (jsaddleSrc + "/jsaddle-webkit2gtk") { };
  jsaddle-webkitgtk = self.callCabal2nix "jsaddle-webkitgtk" (jsaddleSrc + "/jsaddle-webkitgtk") { };
  jsaddle-wkwebview = overrideCabal (self.callCabal2nix "jsaddle-wkwebview" (jsaddleSrc + "/jsaddle-wkwebview") { }) (drv: {
    libraryFrameworkDepends = (drv.libraryFrameworkDepends or [ ]) ++
      (if nixpkgs.stdenv.hostPlatform.useiOSPrebuilt then [
        "${nixpkgs.buildPackages.darwin.xcode}/Contents/Developer/Platforms/${nixpkgs.stdenv.hostPlatform.xcodePlatform}.platform/Developer/SDKs/${nixpkgs.stdenv.hostPlatform.xcodePlatform}.sdk/System"
      ] else (with nixpkgs.buildPackages.darwin.apple_sdk.frameworks; [ Cocoa WebKit ]));
    buildDepends = lib.optional (!nixpkgs.stdenv.hostPlatform.useiOSPrebuilt) [ nixpkgs.buildPackages.darwin.cf-private ];
  });

  # another broken test
  # phantomjs has issues with finding the right port
  # jsaddle-warp = dontCheck (addTestToolDepend (self.callCabal2nix "jsaddle-warp" "${jsaddleSrc}/jsaddle-warp" {}));
  jsaddle-warp = dontCheck (self.callCabal2nix "jsaddle-warp" (jsaddleSrc + "/jsaddle-warp") { });

  jsaddle-dom = doJailbreak (self.callCabal2nix "jsaddle-dom" self._dep.jsaddle-dom { });
  jsaddle-wasm = self.callCabal2nix "jsaddle-wasm" (hackGet (wasmCross + "/jsaddle-wasm")) { };
  ghcjs-dom = self.callCabal2nix "ghcjs-dom" (self._dep.ghcjs-dom + "/ghcjs-dom") { };
  ghcjs-dom-jsaddle = self.callCabal2nix "ghcjs-dom-jsaddle" (self._dep.ghcjs-dom + "/ghcjs-dom-jsaddle") { };
  ghcjs-dom-jsffi = self.callCabal2nix "ghcjs-dom-jsffi" (self._dep.ghcjs-dom + "/ghcjs-dom-jsffi") { };

  ##
  ## Gargoyle and dependencies
  ##

  gargoyle = self.callCabal2nixWithOptions "gargoyle" gargoyleSrc "--subpath gargoyle" { };
  gargoyle-postgresql = haskellLib.overrideCabal
    (self.callCabal2nixWithOptions "gargoyle-postgresql" gargoyleSrc "--subpath gargoyle-postgresql" { })
    (drv: {
      testSystemDepends = (drv.testSystemDepends or [ ]) ++ [ nixpkgs.postgresql_10 ];
    });
  gargoyle-postgresql-nix = haskellLib.overrideCabal
    (self.callCabal2nixWithOptions "gargoyle-postgresql-nix" gargoyleSrc "--subpath gargoyle-postgresql-nix" { })
    (drv: {
      librarySystemDepends = (drv.librarySystemDepends or [ ]) ++ [ nixpkgs.postgresql_10 ];
    });
  gargoyle-postgresql-connect = self.callCabal2nixWithOptions "gargoyle-postgresql-connect" gargoyleSrc "--subpath gargoyle-postgresql-connect" { };
  which = self.callHackage "which" "0.2" { };

  ##
  ## Misc other dependencies
  ##

  haskell-gi-overloading = dontHaddock (self.callHackage "haskell-gi-overloading" "0.0" { });
  monoidal-containers = self.callHackage "monoidal-containers" "0.6.2.0" { };
  patch = self.callCabal2nix "patch" self._dep.patch { };
  commutative-semigroups = self.callCabal2nix "commutative-semigroups" self._dep.commutative-semigroups { };
  witherable = self.callHackage "witherable" "0.4.2" { };

  webdriver = markUnbroken (self.callHackage "webdriver" "0.9.0.1" { });

  # Not on Hackage yet
  # Version 1.2.1 not on Hackage yet
  hspec-webdriver = self.callCabal2nix "hspec-webdriver"
    (fetchFromGitHub {
      owner = "dfordivam";
      repo = "hspec-webdriver-clone";
      rev = "0d748b7bb7cd74dce0a55a1ec86b01dbb8a71cd8";
      sha256 = "1criynifhvmnqwhrshmzylikqkvlgq98xf72w9cdd2zpjw539qf0";
    })
    { };

  constraints-extras = self.callHackage "constraints-extras" "0.3.2.1" { };
  some = self.callHackage "some" "1.0.2" { };
  prim-uniq = self.callHackage "prim-uniq" "0.2" { };
  aeson-gadt-th = self.callHackage "aeson-gadt-th" "0.2.4" { };
  dependent-map = self.callHackage "dependent-map" "0.4.0.0" { };
  dependent-monoidal-map = self.callCabal2nix "dependent-monoidal-map" self._dep.dependent-monoidal-map { };
  dependent-sum = self.callHackage "dependent-sum" "0.7.1.0" { };
  dependent-sum-template = self.callHackage "dependent-sum-template" "0.1.1.0" { };
  dependent-sum-universe-orphans = self.callCabal2nix "dependent-sum-universe-orphans" self._dep.dependent-sum-universe-orphans { };
  dependent-sum-aeson-orphans = self.callHackage "dependent-sum-aeson-orphans" "0.3.0.0" { };

  # Need to use `--subpath` because LICENSE in each dir is a symlink to the repo root.
  universe = self.callCabal2nixWithOptions "universe" universeRepo "--subpath universe" { };
  universe-base = self.callCabal2nixWithOptions "universe" universeRepo "--subpath universe-base" { };
  universe-dependent-sum = nixpkgs.haskell.lib.doJailbreak (self.callCabal2nixWithOptions "universe" universeRepo "--subpath universe-dependent-sum" { });
  universe-instances-extended = self.callCabal2nixWithOptions "universe" universeRepo "--subpath universe-instances-extended" { };
  universe-reverse-instances = self.callCabal2nixWithOptions "universe" universeRepo "--subpath universe-reverse-instances" { };
  universe-instances-base = self.callCabal2nixWithOptions "universe" universeRepo "--subpath deprecated/universe-instances-base" { };

  th-abstraction = self.callHackage "th-abstraction" "0.4.3.0" { };

  # Slightly newer version to fix
  # https://github.com/danfran/cabal-macosx/issues/13
  cabal-macosx = self.callHackage "cabal-macosx" "0.2.4.2" { };
}
