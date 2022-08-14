{ nixpkgsFunc ? import ./nixpkgs
, system ? builtins.currentSystem
, config ? {}
, enableLibraryProfiling ? false
, enableExposeAllUnfoldings ? true
, enableTraceReflexEvents ? false
, useFastWeak ? true
, useReflexOptimizer ? false
, useTextJSString ? true # Use an implementation of "Data.Text" that uses the more performant "Data.JSString" from ghcjs-base under the hood.
, __useTemplateHaskell ? true # Deprecated, just here until we remove feature from reflex and stop CIing it
, iosSdkVersion ? "13.2"
, nixpkgsOverlays ? []
, haskellOverlays ? [] # TODO deprecate
, haskellOverlaysPre ? []
, haskellOverlaysPost ? haskellOverlays
}:

let iosSupport = system == "x86_64-darwin";
    androidSupport = lib.elem system [ "x86_64-linux" ];

    xcodeVer = {
      "13.2" = "11.3.1";
    }.${iosSdkVersion} or (throw "Unknown iosSdkVersion: ${iosSdkVersion}");

    # Overlay for GHC which supports the external splices plugin
    splicesEval = self: super: {
      haskell = super.haskell // {
        compiler = super.haskell.compiler // {
          ghcSplices-8_10 = (super.haskell.compiler.ghc8107.override {
            # New option for GHC 8.10. Explicitly enable profiling builds
            # enableProfiledLibs = true;
            enableDocs = false;
            #libiconv = nixpkgs.libiconv;
          }).overrideAttrs (drv: {
            src = nixpkgs.hackGet ./haskell-overlays/splices-load-save/dep/ghc-8.10;
            # When building from the ghc git repo, ./boot must be run before configuring, whereas
            # in the distribution tarball on the haskell.org downloads page, ./boot has already been
            # run.
            preConfigure= ''
               echo ${drv.version} >VERSION
               ./boot
            '' + drv.preConfigure or "";
            patches = [
              # Patch libraries/unix/config.sub to fix android build
              #./nixpkgs-overlays/android-8.10-splices.patch
            ];
          });
        };
        packages = super.haskell.packages // {
          ghcSplices-8_10 = super.haskell.packages.ghc8107.override {
            buildHaskellPackages = self.buildPackages.haskell.packages.ghcSplices-8_10;
            ghc = self.buildPackages.haskell.compiler.ghcSplices-8_10;
          };
        };
      };
    };

    hackGetOverlay = self: super:
      import ./nixpkgs-overlays/hack-get { inherit lib; } self;

    bindHaskellOverlays = self: super: {
      haskell = super.haskell // {
        overlays = super.haskell.overlays or {} // import ./haskell-overlays {
          nixpkgs = self;
          inherit (self) lib;
          haskellLib = self.haskell.lib;
          inherit
            useFastWeak useReflexOptimizer enableLibraryProfiling enableTraceReflexEvents
            useTextJSString enableExposeAllUnfoldings __useTemplateHaskell
            haskellOverlaysPre
            haskellOverlaysPost;
          inherit  ghcSavedSplices-8_10;
        };
      };
    };

    forceStaticLibs = self: super: {
      darwin = super.darwin // {
        libiconv = super.darwin.libiconv.overrideAttrs (_:
          lib.optionalAttrs (self.stdenv.hostPlatform != self.stdenv.buildPlatform) {
            postInstall = "rm $out/include/libcharset.h $out/include/localcharset.h";
            configureFlags = ["--disable-shared" "--enable-static"];
          });
      };
      zlib = super.zlib.override (lib.optionalAttrs
        (self.stdenv.hostPlatform != self.stdenv.buildPlatform)
        { static = true; shared = false; });
    };

    mobileGhcOverlay = import ./nixpkgs-overlays/mobile-ghc { inherit lib nixpkgsCross; pkgs = nixpkgs; };

    allCabalHashesOverlay = import ./nixpkgs-overlays/all-cabal-hashes;

    nixpkgsArgs = {
      inherit system;
      overlays = [
        hackGetOverlay
        bindHaskellOverlays
        forceStaticLibs
        splicesEval
        mobileGhcOverlay
        allCabalHashesOverlay
        (import ./nixpkgs-overlays/ghc.nix { inherit lib; pkgs = nixpkgs; })
      ] ++ nixpkgsOverlays;
      config = config // {
        permittedInsecurePackages = (config.permittedInsecurePackages or []) ++ [
          "webkitgtk-2.4.11"
        ];

        # XCode needed for native macOS app
        # Obelisk needs it to for some reason
        allowUnfree = true;
      };
    };

    nixpkgs = nixpkgsFunc nixpkgsArgs;

    inherit (nixpkgs) lib fetchurl fetchgit fetchgitPrivate fetchFromGitHub fetchFromBitbucket;

    wasmCross = nixpkgs.hackGet ./wasm-cross;
    webGhcSrc = (import (wasmCross + /webghc.nix) { inherit fetchgit; }).ghc865SplicesSrc;
    nixpkgsCross = {
      android = lib.mapAttrs (_: args: nixpkgsFunc (nixpkgsArgs // args)) rec {
        aarch64 = {
          crossSystem = lib.systems.examples.aarch64-android-prebuilt // {
            #isStatic = true;
          };

        };
        aarch32 = {
           crossSystem = lib.systems.examples.armv7a-android-prebuilt // {
             # Choose an old version so it's easier to find phones to test on
             sdkVer = "23";
           };
         };
      };
      ios = lib.mapAttrs (_: args: nixpkgsFunc (nixpkgsArgs // args)) rec {
        simulator64 = {
          crossSystem = lib.systems.examples.iphone64-simulator // {
            sdkVer = iosSdkVersion;
            inherit xcodeVer;
          };
        };
        aarch64 = {
          crossSystem = lib.systems.examples.iphone64 // {
            sdkVer = iosSdkVersion;
            inherit xcodeVer;
          };
        };
        aarch32 = {
          crossSystem = lib.systems.examples.iphone32 // {
            sdkVer = iosSdkVersion;
            inherit xcodeVer;
          };
        };
        # Back compat
        arm64 = lib.warn "nixpkgsCross.ios.arm64 has been deprecated, using nixpkgsCross.ios.aarch64 instead." aarch64;
      };
      ghcjs = nixpkgsFunc (nixpkgsArgs // {
        crossSystem = lib.systems.examples.ghcjs;
      });
      wasm = nixpkgsFunc (nixpkgsArgs //
        (import wasmCross { inherit nixpkgsFunc; }).nixpkgsCrossArgs webGhcSrc "8.6.5"
      );
    };

    haskellLib = nixpkgs.haskell.lib;

    overrideCabal = pkg: f: if pkg == null then null else haskellLib.overrideCabal pkg f;

    combineOverrides = old: new: old // new // lib.optionalAttrs (old ? overrides && new ? overrides) {
      overrides = lib.composeExtensions old.overrides new.overrides;
    };

    # Makes sure that old `overrides` from a previous call to `override` are not
    # forgotten, but composed. Do this by overriding `override` and passing a
    # function which takes the old argument set and combining it. What a tongue
    # twister!
    makeRecursivelyOverridable = x: x // {
      override = new: makeRecursivelyOverridable (x.override (old: (combineOverrides old new)));
    };

    cabal2nixResult = src: builtins.trace "cabal2nixResult is deprecated; use ghc.haskellSrc2nix or ghc.callCabal2nix instead" (ghc.haskellSrc2nix {
      name = "for-unknown-package";
      src = "file://${src}";
      sha256 = null;
    });

  ghcSavedSplices = ghcSavedSplices-8_10;
  ghcSavedSplices-8_10 = (makeRecursivelyOverridable nixpkgs.haskell.packages.integer-simple.ghcSplices-8_10).override {
    overrides = lib.foldr lib.composeExtensions (_: _: {}) (let
      haskellOverlays = nixpkgs.haskell.overlays;
    in [
      haskellOverlays.combined
      (haskellOverlays.saveSplices "8.10")
      (self: super: with haskellLib; {
        blaze-textual = haskellLib.enableCabalFlag super.blaze-textual "integer-simple";
        cryptonite = disableCabalFlag super.cryptonite "integer-gmp";
        integer-logarithms = disableCabalFlag super.integer-logarithms "integer-gmp";
        scientific = enableCabalFlag super.scientific "integer-simple";
      })
    ]);
  };
  ghcjs = ghcjs8_10;
  ghcjs8_10 = (makeRecursivelyOverridable nixpkgs.haskell.packages.ghcjs810).override {
    overrides = nixpkgsCross.ghcjs.haskell.overlays.combined;
  };

  wasm = ghcWasm32-8_6;
  ghcWasm32-8_6 = makeRecursivelyOverridableBHPToo ((makeRecursivelyOverridable (nixpkgsCross.wasm.haskell.packages.ghcWasm.override (old: {
    # Due to the splices changes the parallel build fails while building the libraries
    ghc = old.ghc.overrideAttrs (drv: { enableParallelBuilding = false; });
  }))).override {
    overrides = nixpkgsCross.wasm.haskell.overlays.combined;
  });

  ghc = ghc8_10;
  ghcHEAD = (makeRecursivelyOverridable nixpkgs.haskell.packages.ghcHEAD).override {
    overrides = nixpkgs.haskell.overlays.combined;
  };
  ghc8_10 = (makeRecursivelyOverridable nixpkgs.haskell.packages.ghc8107).override {
    overrides = nixpkgs.haskell.overlays.combined;
  };

  # Takes a package set with `makeRecursivelyOverridable` and ensures that any
  # future overrides will be applied to both the package set itself and it's
  # build-time package set (`buildHaskellPackages`).
  makeRecursivelyOverridableBHPToo = x: x // {
    override = new: makeRecursivelyOverridableBHPToo (x.override
      (combineOverrides
        {
          overrides = self: super: {
            buildHaskellPackages = super.buildHaskellPackages.override new;
          };
        }
        new));
  };

  ghcAndroidAarch64 = ghcAndroidAarch64-8_10;
  ghcAndroidAarch64-8_10 = makeRecursivelyOverridableBHPToo ((makeRecursivelyOverridable nixpkgsCross.android.aarch64.haskell.packages.integer-simple.ghcSplices-8_10).override {
    overrides = nixpkgsCross.android.aarch64.haskell.overlays.combined;
  });
  ghcAndroidAarch32 = ghcAndroidAarch32-8_10;
  ghcAndroidAarch32-8_10 = makeRecursivelyOverridableBHPToo ((makeRecursivelyOverridable nixpkgsCross.android.aarch32.haskell.packages.integer-simple.ghcSplices-8_10).override {
     overrides = nixpkgsCross.android.aarch32.haskell.overlays.combined;
  });

  ghcIosSimulator64 = ghcIosSimulator64-8_10;
  ghcIosSimulator64-8_10 = makeRecursivelyOverridableBHPToo ((makeRecursivelyOverridable nixpkgsCross.ios.simulator64.haskell.packages.integer-simple.ghcSplices-8_10).override {
    overrides = nixpkgsCross.ios.simulator64.haskell.overlays.combined;
  });
  ghcIosAarch64 = ghcIosAarch64-8_10;
  ghcIosAarch64-8_10 = makeRecursivelyOverridableBHPToo ((makeRecursivelyOverridable nixpkgsCross.ios.aarch64.haskell.packages.integer-simple.ghcSplices-8_10).override {
    overrides = nixpkgsCross.ios.aarch64.haskell.overlays.combined;
  });
  ghcIosAarch32 = ghcIosAarch32-8_10;
  ghcIosAarch32-8_10 = makeRecursivelyOverridableBHPToo ((makeRecursivelyOverridable nixpkgsCross.ios.aarch32.haskell.packages.integer-simple.ghcSplices-8_10).override {
    overrides = nixpkgsCross.ios.aarch32.haskell.overlays.combined;
  });

  #TODO: Separate debug and release APKs
  #TODO: Warn the user that the android app name can't include dashes
  android = androidWithHaskellPackages {
    inherit ghcAndroidAarch64 ghcAndroidAarch32;
  };
  android-8_10 = androidWithHaskellPackages {
    ghcAndroidAarch64 = ghcAndroidAarch64-8_10;
    ghcAndroidAarch32 = ghcAndroidAarch32-8_10;
  };
  androidWithHaskellPackages = { ghcAndroidAarch64, ghcAndroidAarch32 }: import ./android {
    inherit nixpkgs nixpkgsCross ghcAndroidAarch64 ghcAndroidAarch32 overrideCabal;
    acceptAndroidSdkLicenses = config.android_sdk.accept_license or false;
  };
  iosAarch64 = iosWithHaskellPackages ghcIosAarch64;
  iosAarch64-8_10 = iosWithHaskellPackages ghcIosAarch64-8_10;
  iosAarch32 = iosWithHaskellPackages ghcIosAarch32;
  iosAarch32-8_10 = iosWithHaskellPackages ghcIosAarch32-8_10;
  iosSimulator = {
    buildApp = nixpkgs.lib.makeOverridable (import ./ios { inherit nixpkgs; ghc = ghcIosSimulator64; withSimulator = true; });
  };
  iosWithHaskellPackages = ghc: {
    buildApp = nixpkgs.lib.makeOverridable (import ./ios { inherit nixpkgs ghc; });
  };

in let this = rec {
  inherit (nixpkgs)
    filterGit
    hackGet
    thunkSet
    ;
  inherit nixpkgs
          nixpkgsCross
          overrideCabal
          ghc
          ghcHEAD
          ghc8_10
          ghcIosSimulator64
          ghcIosAarch64
          ghcIosAarch64-8_10
          ghcIosAarch32
          ghcIosAarch32-8_10
          ghcAndroidAarch64
          ghcAndroidAarch64-8_10
          ghcAndroidAarch32
          ghcAndroidAarch32-8_10
          ghcjs
          ghcjs8_10
          ghcSavedSplices
          ghcSavedSplices-8_10
          android
          androidWithHaskellPackages
          iosAarch32
          iosAarch64
          iosSimulator
          iosWithHaskellPackages
          wasm
          wasmCross
          ;

  # Back compat
  ios = iosAarch64;
  ghcAndroidArm64 = lib.warn "ghcAndroidArm64 has been deprecated, using ghcAndroidAarch64 instead." ghcAndroidAarch64;
  ghcAndroidArmv7a = lib.warn "ghcAndroidArmv7a has been deprecated, using ghcAndroidAarch32 instead." ghcAndroidAarch32;
  ghcIosArm64 = lib.warn "ghcIosArm64 has been deprecated, using ghcIosAarch64 instead." ghcIosAarch64;

  androidReflexTodomvc = android.buildApp {
    package = p: p.reflex-todomvc;
    executableName = "reflex-todomvc";
    applicationId = "org.reflexfrp.todomvc";
    displayName = "Reflex TodoMVC";
  };
  androidReflexTodomvc-8_10 = android-8_10.buildApp {
    package = p: p.reflex-todomvc;
    executableName = "reflex-todomvc";
    applicationId = "org.reflexfrp.todomvc.via_8_10";
    displayName = "Reflex TodoMVC via GHC 8.10";
  };
  iosReflexTodomvc = ios.buildApp {
    package = p: p.reflex-todomvc;
    executableName = "reflex-todomvc";
    bundleIdentifier = "org.reflexfrp.todomvc";
    bundleName = "Reflex TodoMVC";
  };
  iosReflexTodomvc-8_10 = iosAarch64-8_10.buildApp {
    package = p: p.reflex-todomvc;
    executableName = "reflex-todomvc";
    bundleIdentifier = "org.reflexfrp.todomvc.via_8_10";
    bundleName = "Reflex TodoMVC via GHC 8.10";
  };
  iosSimulatorReflexTodomvc = iosSimulator.buildApp {
    package = p: p.reflex-todomvc;
    executableName = "reflex-todomvc";
    bundleIdentifier = "org.reflexfrp.todomvc";
    bundleName = "Reflex TodoMVC";
  };
  setGhcLibdir = ghcLibdir: inputGhcjs:
    let libDir = "$out/lib/ghcjs-${inputGhcjs.version}";
        ghcLibdirLink = nixpkgs.stdenv.mkDerivation {
          name = "ghc_libdir";
          inherit ghcLibdir;
          buildCommand = ''
            mkdir -p ${libDir}
            echo "$ghcLibdir" > ${libDir}/ghc_libdir_override
          '';
        };
    in inputGhcjs // {
    outPath = nixpkgs.buildEnv {
      inherit (inputGhcjs) name;
      paths = [ inputGhcjs ghcLibdirLink ];
      postBuild = ''
        mv ${libDir}/ghc_libdir_override ${libDir}/ghc_libdir
      '';
    };
  };

  platforms = [
    "ghcjs"
    "ghc"
  ];

  androidDevTools = [
    ghc.haven
    nixpkgs.maven
    (import ./android/androidComposition.nix { inherit nixpkgs;  })
    ];
  # Tools that are useful for development under both ghc and ghcjs
  generalDevTools' = { nativeHaskellPackages ? ghc }: {
    inherit (nativeHaskellPackages)
      Cabal
      cabal-install
      ghcid
      hasktags
      hlint
      stylish-haskell # Recent stylish-haskell only builds with AMP in place
      reflex-ghci
      ;
    inherit (nixpkgs)
      cabal2nix
      curl
      nix-prefetch-scripts
      nodejs
      pkgconfig
      closurecompiler
      ;
  };

  workOn = haskellPackages: package: (overrideCabal package (drv: {
    buildTools = (drv.buildTools or []) ++ builtins.attrValues (generalDevTools' {});
  })).env;

  # A minimal wrapper around the build-wasm-app from wasm-cross
  # Useful in building simple cabal projects like reflex-todomvc
  build-wasm-app-wrapper =
    ename: # Name of the executable, usually same as cabal project name
    pkgPath : # Path of cabal package
    args: # Others options to pass to build-wasm-app
  let
    pkg = wasm.callPackage pkgPath {};
    webabi = nixpkgs.callPackage (wasmCross + /webabi) {};
    build-wasm-app = nixpkgs.callPackage (wasmCross + /build-wasm-app.nix) ({ inherit webabi; } // args);
  in build-wasm-app {
    inherit pkg ename;
  };

  # A simple derivation that just creates a file with the names of all
  # of its inputs. If built, it will have a runtime dependency on all
  # of the given build inputs.
  pinBuildInputs = name: buildInputs: (nixpkgs.releaseTools.aggregate {
    inherit name;
    constituents = buildInputs;
  }).overrideAttrs (old: {
    buildCommand = old.buildCommand + ''
      echo "$propagatedBuildInputs $buildInputs $nativeBuildInputs $propagatedNativeBuildInputs" > "$out/deps"
    '';
    inherit buildInputs;
  });

  reflexEnv = platform:
    let haskellPackages = builtins.getAttr platform this;
        ghcWithStuff = if platform == "ghc" # || platform == "ghcjs"
                       then haskellPackages.ghcWithHoogle
                       else haskellPackages.ghcWithPackages;
    in ghcWithStuff (p: import ./packages.nix {
      haskellPackages = p;
      inherit platform;
    });

  tryReflexPackages = builtins.attrValues (generalDevTools' {})
    ++ builtins.map reflexEnv platforms;

  cachePackages =
    let otherPlatforms = lib.optionals androidSupport [
          "ghcAndroidAarch64"
          "ghcAndroidAarch32"
        ] ++ lib.optionals iosSupport [
          "ghcIosAarch64"
          "ghcIosSimulator64"
        ];
    in tryReflexPackages
      ++ builtins.map reflexEnv otherPlatforms
      ++ lib.optionals androidSupport [
        androidDevTools
        androidReflexTodomvc
      ] ++ lib.optionals iosSupport [
        iosReflexTodomvc iosSimulatorReflexTodomvc
      ];

  inherit cabal2nixResult system androidSupport iosSupport;
  project = args: import ./project this (args ({ pkgs = nixpkgs; } // this));
  tryReflexShell = pinBuildInputs ("shell-" + system) tryReflexPackages;
  ghcjsExternsJs = ./ghcjs.externs.js;

};

in this
