{ haskellLib
, lib, nixpkgs
, thunkSet, fetchFromGitHub, fetchFromBitbucket, hackGet
, useFastWeak, useReflexOptimizer, enableTraceReflexEvents, enableLibraryProfiling, __useTemplateHaskell
, useWebkit2Gtk
}:

with haskellLib.compose;

self: super:

let
  reflexOptimizerFlag = lib.optional (useReflexOptimizer && (self.ghc.cross or null) == null) "-fuse-reflex-optimizer";
  useTemplateHaskellFlag = lib.optional (!__useTemplateHaskell) "-f-use-template-haskell";
  useWebkit2GtkFlag = if useWebkit2Gtk
    then ["-fwebkit2gtk"]
    else ["-f-webkit2gtk"] ++ lib.optional ((nixpkgs.stdenv.hostPlatform.isLinux or false) && !nixpkgs.stdenv.hostPlatform.useAndroidPrebuilt) "-fuse-warp"; # Enable warp on linux if webkit2gtk is disabled. Other platforms have other default runners

in
{
  _dep = super._dep or {} // thunkSet ./dep;

  ##
  ## Reflex family
  ##

  #reflex = self.callCabal2nixWithOptions "reflex" self._dep.reflex (lib.concatStringsSep " " (lib.concatLists [
  #  (lib.optional enableTraceReflexEvents "-fdebug-trace-events")
  #  reflexOptimizerFlag
  #  useTemplateHaskellFlag
  #  (lib.optional useFastWeak "-ffast-weak")
  #])) {};

  #reflex-todomvc =
  #  let
  #    flags =
  #      if useWebkit2Gtk && nixpkgs.stdenv.hostPlatform.isLinux
  #      then [ "-f-warp" "-f-webkitgtk" "-f-wkwebview" ]
  #      else if (nixpkgs.stdenv.hostPlatform.isLinux && !nixpkgs.stdenv.hostPlatform.useAndroidPrebuilt)
  #      then [ "-fwarp" "-f-webkitgtk" "-f-wkwebview" "-f-webkit2gtk" ]
  #      else if self.ghc.stdenv.targetPlatform.isiOS
  #      then [ "-f-webkit2gtk" "-f-warp" "-f-webkitgtk" ]
  #      else if nixpkgs.stdenv.hostPlatform.isDarwin
  #      then [ "-fwkwebview" "-f-webkit2gtk" "-f-webkitgtk" ]
  #      else [];
  #  in
  #    (haskellLib.doJailbreak (self.callCabal2nixWithOptions "reflex-todomvc" self._dep.reflex-todomvc (lib.concatStringsSep " " flags) {}));
  #reflex-aeson-orphans = self.callCabal2nix "reflex-aeson-orphans" self._dep.reflex-aeson-orphans {};

  ## The tests for reflex-dom-core are not deterministic, disable them, and run them manually
  #reflex-dom-core = let
  #  inherit (self) ghc;
  #  noGcTest = stdenv.hostPlatform.system != "x86_64-linux"
  #          || stdenv.hostPlatform != stdenv.buildPlatform
  #          || stdenv.targetPlatform.isiOS
  #          || (ghc.isGhcjs or false);
  #in haskellLib.overrideCabal
  #  (self.callCabal2nixWithOptions "reflex-dom-core" (reflexDomRepo + "/reflex-dom-core") (lib.concatStringsSep " " (lib.concatLists [
  #    reflexOptimizerFlag
  #    useTemplateHaskellFlag
  #    (lib.optional enableLibraryProfiling "-fprofile-reflex")
  #    [ "-f-hydration-tests" ]
  #    [ "-f-gc-tests" ]
  #  ])) {})
  #  (drv: {
  #    # TODO: Get hlint working for cross-compilation
  #    #doCheck = stdenv.hostPlatform == stdenv.buildPlatform && !(ghc.isGhcjs or false);
  #    doCheck = false;
  #    # The headless browser run as part of the tests will exit without this
  #    preBuild = (drv.preBuild or "") + ''
  #      export HOME="$PWD"
  #    '';

  #    # Show some output while running tests, so we might notice what's wrong
  #    testTarget = "--show-details=streaming";

  #    testHaskellDepends = with self; (drv.testHaskellDepends or []) ++ lib.optionals (!noGcTest) [
  #      temporary
  #      jsaddle-warp
  #      process
  #      chrome-test-utils
  #    ];

  #    testSystemDepends = with nixpkgs; (drv.testSystemDepends or []) ++ lib.optionals (nixpkgs.stdenv.hostPlatform.isLinux) [
  #      nixpkgs_oldChromium.selenium-server-standalone
  #      nixpkgs_oldChromium.chromium
  #      which
  #    ] ++ lib.optionals (!noGcTest) [
  #      nixpkgs.iproute2
  #    ];
  #  } // lib.optionalAttrs (!noGcTest) {
  #    # The headless browser run as part of gc tests would hang/crash without this
  #    preCheck = ''
  #      export FONTCONFIG_PATH=${nixpkgs.fontconfig.out}/etc/fonts
  #    '' + (drv.preCheck or "");
  #  });

  reflex-dom = lib.pipe super.reflex-dom [
    (appendConfigureFlags [reflexOptimizerFlag useTemplateHaskellFlag useWebkit2GtkFlag])
    (if useWebkit2Gtk then lib.id else addBuildDepends [self.jsaddle-warp])
    (x: x.override {
      jsaddle-webkit2gtk = if useWebkit2Gtk
        then self.jsaddle-webkit2gtk
        else null;
     })
    ];

  }
