{ nixpkgsFunc ? import ./nixpkgs
, system ? builtins.currentSystem
, config ? {}
, enableLibraryProfiling ? false
, enableExposeAllUnfoldings ? true
, enableTraceReflexEvents ? false
, useFastWeak ? true
, useReflexOptimizer ? false
, useTextJSString ? true
, iosSdkVersion ? "10.2"
, iosSdkLocation ? "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS${iosSdkVersion}.sdk"
, iosSupportForce ? false
}:
let iosSupport =
      if system != "x86_64-darwin" then false
      else if iosSupportForce || builtins.pathExists iosSdkLocation then true
      else builtins.trace "Warning: No iOS sdk found at ${iosSdkLocation}; iOS support disabled.  To enable, either install a version of Xcode that provides that SDK or override the value of iosSdkVersion to match your installed version." false;
    globalOverlay = self: super: {
      all-cabal-hashes = fetchurl {
        url = https://github.com/commercialhaskell/all-cabal-hashes/archive/c7af4479644dd1657df956dd7575b070c1e30d83.tar.gz;
        sha256 = "1aw6lcyjlfcpk74al489gds4vr4709d0rpchrr0lysrpk7mk2a7g";
      };
    };
    appleLibiconvHack = self: super: {
      darwin = super.darwin // {
        libiconv =
          if self.hostPlatform == self.buildPlatform
          then super.darwin.libiconv
          else super.darwin.libiconv.overrideAttrs (o: {
            postInstall = "rm $out/include/libcharset.h $out/include/localcharset.h";
            configureFlags = ["--disable-shared" "--enable-static"];
        });
      };
    };
    nixpkgs = nixpkgsFunc ({
      inherit system;
      overlays = [globalOverlay];
      config = {
        allowUnfree = true;
        allowBroken = true; # GHCJS is marked broken in 011c149ed5e5a336c3039f0b9d4303020cff1d86
        permittedInsecurePackages = [
          "webkitgtk-2.4.11"
        ];
        packageOverrides = pkgs: {
          webkitgtk = pkgs.webkitgtk220x;
          # cabal2nix's tests crash on 32-bit linux; see https://github.com/NixOS/cabal2nix/issues/272
          ${if system == "i686-linux" then "cabal2nix" else null} = pkgs.haskell.lib.dontCheck pkgs.cabal2nix;
        };
      } // config;
    });
    inherit (nixpkgs) fetchurl fetchgit fetchgitPrivate fetchFromGitHub;
    nixpkgsCross = {
      android = nixpkgs.lib.mapAttrs (_: args: if args == null then null else nixpkgsFunc args) rec {
        aarch64 = {
          system = "x86_64-linux";
          overlays = [globalOverlay];
          crossSystem = nixpkgs.lib.systems.examples.aarch64-android-prebuilt;
          config.allowUnfree = true;
        };
        aarch32 = {
          system = "x86_64-linux";
          overlays = [globalOverlay];
          crossSystem = nixpkgs.lib.systems.examples.armv7a-android-prebuilt;
          config.allowUnfree = true;
        };
        # Back compat
        arm64Impure = aarch64;
        armv7aImpure = aarch32;
      };
      ios =
        let config = { allowUnfree = true; };
        in nixpkgs.lib.mapAttrs (_: args: if args == null then null else nixpkgsFunc args) rec {
        simulator64 = {
          system = "x86_64-darwin";
          overlays = [globalOverlay appleLibiconvHack];
          crossSystem = nixpkgs.lib.systems.examples.iphone64-simulator // {
            sdkVer = iosSdkVersion;
          };
          inherit config;
        };
        aarch64 = {
          system = "x86_64-darwin";
          overlays = [globalOverlay appleLibiconvHack];
          crossSystem = nixpkgs.lib.systems.examples.iphone64 // {
            sdkVer = iosSdkVersion;
          };
          inherit config;
        };
        aarch32 = {
          system = "x86_64-darwin";
          overlays = [globalOverlay appleLibiconvHack ];
          crossSystem = nixpkgs.lib.systems.examples.iphone32 // {
            sdkVer = iosSdkVersion;
          };
          inherit config;
        };
        # Back compat
        arm64 = aarch64;
        armv7 = aarch32;
      };
    };
    haskellLib = nixpkgs.haskell.lib;
    filterGit = builtins.filterSource (path: type: !(builtins.any (x: x == baseNameOf path) [".git" "tags" "TAGS" "dist"]));
    # Retrieve source that is controlled by the hack-* scripts; it may be either a stub or a checked-out git repo
    hackGet = p:
      if builtins.pathExists (p + "/git.json") then (
        let gitArgs = builtins.fromJSON (builtins.readFile (p + "/git.json"));
        in if builtins.elem "@" (nixpkgs.lib.stringToCharacters gitArgs.url)
        then fetchgitPrivate gitArgs
        else fetchgit gitArgs)
      else if builtins.pathExists (p + "/github.json") then fetchFromGitHub (builtins.fromJSON (builtins.readFile (p + "/github.json")))
      else {
        name = baseNameOf p;
        outPath = filterGit p;
      };
    inherit (nixpkgs.stdenv.lib) optional optionals optionalAttrs;
    optionalExtension = cond: overlay: if cond then overlay else _: _: {};
    applyPatch = patch: src: nixpkgs.runCommand "applyPatch" {
      inherit src patch;
    } ''
      cp -r "$src" "$out"

      cd "$out"
      chmod -R +w .
      patch -p1 <"$patch"
    '';
in with haskellLib;
let overrideCabal = pkg: f: if pkg == null then null else haskellLib.overrideCabal pkg f;
    replaceSrc = pkg: src: version: overrideCabal pkg (drv: {
      inherit src version;
      sha256 = null;
      revision = null;
      editedCabalFile = null;
    });
    combineOverrides = old: new: (old // new) // {
      overrides = nixpkgs.lib.composeExtensions old.overrides new.overrides;
    };
    makeRecursivelyOverridable = x: old: x.override old // {
      override = new: makeRecursivelyOverridable x (combineOverrides old new);
    };
    foreignLibSmuggleHeaders = pkg: overrideCabal pkg (drv: {
      postInstall = ''
        cd dist/build/${pkg.pname}/${pkg.pname}-tmp
        for header in $(find . | grep '\.h'$); do
          local dest_dir=$out/include/$(dirname "$header")
          mkdir -p "$dest_dir"
          cp "$header" "$dest_dir"
        done
      '';
    });
    cabal2nixResult = src: builtins.trace "cabal2nixResult is deprecated; use ghc.haskellSrc2nix or ghc.callCabal2nix instead" (ghc.haskellSrc2nix {
      name = "for-unknown-package";
      src = "file://${src}";
      sha256 = null;
    });
    addReflexTraceEventsFlag = if enableTraceReflexEvents
      then drv: appendConfigureFlag drv "-fdebug-trace-events"
      else drv: drv;
    addFastWeakFlag = if useFastWeak
      then drv: enableCabalFlag drv "fast-weak"
      else drv: drv;
    ghcjsPkgs = ghcjs: self: super: {
      ghcjs = ghcjs.overrideAttrs (o: {
        patches = (o.patches or []) ++ optional useFastWeak ./fast-weak.patch;
      });
      ghci-ghcjs = self.callCabal2nix "ghci-ghcjs" (ghcjsSrc + "/lib/ghci-ghcjs") {};
      ghcjs-th = self.callCabal2nix "ghcjs-th" (ghcjsSrc + "/lib/ghcjs-th") {};
      template-haskell-ghcjs = self.callCabal2nix "template-haskell-ghcjs" (ghcjsSrc + "/lib/template-haskell-ghcjs") {};
      ghc-api-ghcjs = overrideCabal (self.callCabal2nix "ghc-api-ghcjs" (ghcjsSrc + "/lib/ghc-api-ghcjs") {}) (drv: {
        libraryToolDepends = (drv.libraryToolDepends or []) ++ [
          ghc8_2_2.alex
          ghc8_2_2.happy
        ];
      });
      haddock-api-ghcjs = self.callCabal2nix "haddock-api-ghcjs" (ghcjsSrc + "/lib/haddock-api-ghcjs") {};
      haddock-library-ghcjs = dontHaddock (self.callCabal2nix "haddock-library-ghcjs" (ghcjsSrc + "/lib/haddock-library-ghcjs") {});
    };

    extendHaskellPackages = haskellPackages: makeRecursivelyOverridable haskellPackages {
      overrides = self: super:
        let reflexDom = import (hackGet ./reflex-dom) self nixpkgs;
            jsaddlePkgs = import (hackGet ./jsaddle) self;
            gargoylePkgs = self.callPackage (hackGet ./gargoyle) self;
            ghcjsDom = import (hackGet ./ghcjs-dom) self;
            addReflexOptimizerFlag = if useReflexOptimizer && (self.ghc.cross or null) == null
              then drv: appendConfigureFlag drv "-fuse-reflex-optimizer"
              else drv: drv;
        in {

        servant-auth-server = self.callHackage "servant-auth-server" "0.3.1.0" {};
        vector = doJailbreak super.vector;
        these = doJailbreak super.these;
        aeson-compat = doJailbreak super.aeson-compat;
        timezone-series = self.callCabal2nix "timezone-series" (fetchFromGitHub {
          owner = "ygale";
          repo = "timezone-series";
          rev = "9f42baf542c54ad554bd53582819eaa454ed633d";
          sha256 = "1axrx8lziwi6pixws4lq3yz871vxi81rib6cpdl62xb5bh9y03j6";
        }) {};
        timezone-olson = self.callCabal2nix "timezone-olson" (fetchFromGitHub {
          owner = "ygale";
          repo = "timezone-olson";
          rev = "aecec86be48580f23145ffb3bf12a4ae191d12d3";
          sha256 = "1xxbwb8z27qbcscbg5qdyzlc2czg5i3b0y04s9h36hfcb07hasnz";
        }) {};
        quickcheck-instances = doJailbreak super.quickcheck-instances;

        gtk2hs-buildtools = doJailbreak super.gtk2hs-buildtools;

        # hindent was overriden with a newer version of haskell-src-exts for some reason
        hindent = super.hindent.override { haskell-src-exts = self.haskell-src-exts; };
        # Not sure why these tests fail...
        hfmt = dontCheck super.hfmt;

        ########################################################################
        # Reflex packages
        ########################################################################
        reflex = dontHaddock (addFastWeakFlag (addReflexTraceEventsFlag (addReflexOptimizerFlag (self.callPackage (hackGet ./reflex) {}))));
        reflex-dom = addReflexOptimizerFlag (dontHaddock (doJailbreak reflexDom.reflex-dom));
        reflex-dom-core = addReflexOptimizerFlag (dontHaddock (doJailbreak reflexDom.reflex-dom-core));
        reflex-todomvc = self.callPackage (hackGet ./reflex-todomvc) {};
        reflex-aeson-orphans = self.callPackage (hackGet ./reflex-aeson-orphans) {};
        haven = doJailbreak (self.callHackage "haven" "0.2.0.0" {});

        inherit (jsaddlePkgs) jsaddle-clib jsaddle-webkit2gtk jsaddle-webkitgtk;
        jsaddle = doJailbreak jsaddlePkgs.jsaddle;
        jsaddle-warp = dontCheck jsaddlePkgs.jsaddle-warp;

        # HACK(mbauer): Can’t figure out why cf-private framework is
        #               not getting pulled in. This override gives us
        #               a fake CoreFoundation directory that mimics
        #               the framework behavior.
        jsaddle-wkwebview = jsaddlePkgs.jsaddle-wkwebview.overrideAttrs (drv:
          optionalAttrs (nixpkgs.targetPlatform.isDarwin && !nixpkgs.targetPlatform.isiOS) {
            libraryFrameworkDepends = [ nixpkgs.darwin.apple_sdk.frameworks.Cocoa ];
            preSetupCompilerEnvironment = ''
              ${drv.preSetupCompilerEnvironment or ""}
              mkdir include
              ln -s ${nixpkgs.darwin.cf-private}/Library/Frameworks/CoreFoundation.framework/Headers include/CoreFoundation
              export NIX_CFLAGS_COMPILE="-I$PWD/include $NIX_CFLAGS_COMPILE"
            '';
          });

        jsaddle-dom = overrideCabal (self.callPackage (hackGet ./jsaddle-dom) {}) (drv: {
          # On macOS, the jsaddle-dom build will run out of file handles the first time it runs
          preBuild = ''./setup build || true'';
          jailbreak = true;
        });

        inherit (ghcjsDom) ghcjs-dom-jsffi;

        # TODO: Fix this in Cabal
        # When building a package with no haskell files, cabal haddock shouldn't fail
        ghcjs-dom-jsaddle = dontHaddock ghcjsDom.ghcjs-dom-jsaddle;
        ghcjs-dom = dontHaddock ghcjsDom.ghcjs-dom;

        inherit (gargoylePkgs) gargoyle gargoyle-postgresql;

        ${if system == "i686-linux" then "language-nix" else null} = dontCheck super.language-nix;

        ########################################################################
        # Tweaks
        ########################################################################
        haskell-gi = dontCheck super.haskell-gi;
        ghcjs-base-stub = dontHaddock super.ghcjs-base-stub;

        exception-transformers = doJailbreak super.exception-transformers;
        # haskell-src-exts = self.callHackage "haskell-src-exts" "1.20.1" {};
        # haskell-src-meta = self.callHackage "haskell-src-meta" "0.8.0.2" {};

        haskell-gi-overloading = dontHaddock (self.callHackage "haskell-gi-overloading" "0.0" {});

        webkit2gtk3-javascriptcore = super.webkit2gtk3-javascriptcore.override {
          webkitgtk = nixpkgs.webkitgtk220x;
        };

        ########################################################################
        # Fixes to be upstreamed
        ########################################################################
        foundation = dontCheck super.foundation;
        MonadCatchIO-transformers = doJailbreak super.MonadCatchIO-transformers;
        blaze-builder-enumerator = doJailbreak super.blaze-builder-enumerator;
        process-extras = dontCheck super.process-extras;
        miso = addBuildDepend (self.callHackage "miso" "0.12.0.0" {}) self.ghcjs-base;
        hasktags = dontCheck super.hasktags;
        fast-logger = dontCheck super.fast-logger;

        ########################################################################
        # Packages not in hackage
        ########################################################################
        servant-reflex = self.callCabal2nix "servant-reflex" (fetchFromGitHub {
          owner = "imalsogreg";
          repo = "servant-reflex";
          rev = "5cd3098880741e6ade52ef4477422d9c776e5478";
          sha256 = "18yjfamx3k9xd8pz251jsmvhlj4riw0brk2fyvjq00r87cx67a6f";
        }) {};
        concat = dontHaddock (dontCheck (self.callCabal2nix "concat" (fetchFromGitHub {
          owner = "conal";
          repo = "concat";
          rev = "24a4b8ccc883605ea2b0b4295460be2f8a245154";
          sha256 = "0mcwqzjk3f8qymmkbpa80l6mh6aa4vcyxky3gpwbnx19g721mj35";
        }) {}));
        direct-sqlite = self.callCabal2nix "direct-sqlite" (fetchFromGitHub {
          owner = "IreneKnapp";
          repo = "direct-sqlite";
          rev = "cd1ab3c0ee7894d888be826fc653b75813fd53c9";
          sha256 = "13i6lz99x0jb9fgns7brlqnv5s5w4clp26l8c3kxd318r1krvr6w";
        }) {};

        superconstraints =
          # Remove override when assertion fails
          assert (super.superconstraints or null) == null;
          self.callPackage (self.haskellSrc2nix {
            name = "superconstraints";
            src = fetchurl {
              url = "https://hackage.haskell.org/package/superconstraints-0.0.1/superconstraints.cabal";
              sha256 = "0bgc8ldml3533522gp1x2bjiazllknslpl2rvdkd1k1zfdbh3g9m";
            };
            sha256 = "1gx9p9i5jli91dnvvrc30j04h1v2m3d71i8sxli6qrhplq5y63dk";
          }) {};
      } // (if enableLibraryProfiling then {
        mkDerivation = expr: super.mkDerivation (expr // { enableLibraryProfiling = true; });
      } else {});
    };
    haskellOverlays = import ./haskell-overlays {
      inherit
        haskellLib
        nixpkgs jdk fetchFromGitHub
        useReflexOptimizer
        hackGet;
      inherit (nixpkgs) lib;
      androidActivity = hackGet ./android-activity;
    };
    ghcjs8_2_2Packages = nixpkgs.callPackage (nixpkgs.path + "/pkgs/development/haskell-modules") {
      ghc = ghc8_2_2.ghcjs;
      buildHaskellPackages = ghc8_2_2.ghcjs.bootPkgs;
      compilerConfig = nixpkgs.callPackage (nixpkgs.path + "/pkgs/development/haskell-modules/configuration-ghc-7.10.x.nix") { inherit haskellLib; };
      packageSetConfig = nixpkgs.callPackage (nixpkgs.path + "/pkgs/development/haskell-modules/configuration-ghcjs.nix") { inherit haskellLib; };
      inherit haskellLib;
    };
    ghcjs8_4_2Packages = nixpkgs.callPackage (nixpkgs.path + "/pkgs/development/haskell-modules") {
      ghc = ghc8_4_2.ghcjs;
      buildHaskellPackages = ghc8_4_2.ghcjs.bootPkgs;
      compilerConfig = nixpkgs.callPackage (nixpkgs.path + "/pkgs/development/haskell-modules/configuration-ghc-7.10.x.nix") { inherit haskellLib; };
      packageSetConfig = nixpkgs.callPackage (nixpkgs.path + "/pkgs/development/haskell-modules/configuration-ghcjs.nix") { inherit haskellLib; };
      inherit haskellLib;
    };
  ghc = ghc8_4_2;
  ghcjs8_2_2 = (extendHaskellPackages ghcjs8_2_2Packages).override {
    overrides = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions (_: _: {}) [
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      haskellOverlays.ghcjs
      (optionalExtension useTextJSString haskellOverlays.textJSString)
    ];
  };
  ghcjs8_4_2 = (extendHaskellPackages ghcjs8_4_2Packages).override {
    overrides = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions (_: _: {}) [
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      haskellOverlays.ghcjs
      (optionalExtension useTextJSString haskellOverlays.textJSString)
    ];
  };
  ghcjs = ghcjs8_4_2;
  ghcHEAD = (extendHaskellPackages nixpkgs.pkgs.haskell.packages.ghcHEAD).override {
    overrides = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions (_: _: {}) [
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      haskellOverlays.ghc-head
    ];
  };
  ghc8_4_2 = (extendHaskellPackages nixpkgs.pkgs.haskell.packages.ghc842).override {
    overrides = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions (_: _: {}) [
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      (ghcjsPkgs nixpkgs.pkgs.haskell.compiler.ghcjs84)
      haskellOverlays.ghc-8_4_2
    ];
  };
  ghc8_2_2 = (extendHaskellPackages nixpkgs.pkgs.haskell.packages.ghc822).override {
    overrides = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions (_: _: {}) [
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      (ghcjsPkgs nixpkgs.pkgs.haskell.compiler.ghcjs82)
      haskellOverlays.ghc-8_2_2
    ];
  };
  ghc8_0_2 = (extendHaskellPackages nixpkgs.pkgs.haskell.packages.ghc802).override {
    overrides = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions (_: _: {}) [
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      haskellOverlays.ghc-8
    ];
  };
  ghc7 = (extendHaskellPackages nixpkgs.pkgs.haskell.packages.ghc7103).override {
    overrides = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions (_: _: {}) [
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      haskellOverlays.ghc-7
    ];
  };
  ghcAndroidAarch64 = (extendHaskellPackages nixpkgsCross.android.aarch64.pkgs.haskell.packages.integer-simple.ghc822).override {
    overrides = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions (_: _: {}) [
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      haskellOverlays.ghc-8_2_2
      haskellOverlays.disableTemplateHaskell
      haskellOverlays.android
    ];
  };
  ghcAndroidAarch32 = (extendHaskellPackages nixpkgsCross.android.aarch32.pkgs.haskell.packages.integer-simple.ghc822).override {
    overrides = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions (_: _: {}) [
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      haskellOverlays.ghc-8_2_2
      haskellOverlays.disableTemplateHaskell
      haskellOverlays.android
    ];
  };
  ghcIosSimulator64 = (extendHaskellPackages nixpkgsCross.ios.simulator64.pkgs.haskell.packages.ghc842).override {
    overrides = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions (_: _: {}) [
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      haskellOverlays.disableTemplateHaskell
      haskellOverlays.ghc-8_4_2
      haskellOverlays.ios
    ];
  };
  ghcIosAarch64 = (extendHaskellPackages nixpkgsCross.ios.aarch64.pkgs.haskell.packages.ghc842).override {
    overrides = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions (_: _: {}) [
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      haskellOverlays.ghc-8_4_2
      haskellOverlays.disableTemplateHaskell
      haskellOverlays.ios
    ];
  };
  ghcIosAarch32 = (extendHaskellPackages nixpkgsCross.ios.aarch32.pkgs.haskell.packages.ghc842).override {
    overrides = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions (_: _: {}) [
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      haskellOverlays.ghc-8_4_2
      haskellOverlays.disableTemplateHaskell
      haskellOverlays.ios
    ];
  };
  # Back compat
  ghcAndroidArm64 = ghcAndroidAarch64;
  ghcAndroidArmv7a = ghcAndroidAarch32;
  ghcIosArm64 = ghcIosAarch64;
  ghcIosArmv7 = ghcIosAarch32;
  #TODO: Separate debug and release APKs
  #TODO: Warn the user that the android app name can't include dashes
  android = androidWithHaskellPackages { inherit ghcAndroidAarch64 ghcAndroidAarch32; };
  androidWithHaskellPackages = { ghcAndroidAarch64, ghcAndroidAarch32 }: import ./android {
    nixpkgs = nixpkgsFunc { system = "x86_64-linux"; };
    inherit nixpkgsCross ghcAndroidAarch64 ghcAndroidAarch32 overrideCabal;
  };
  nix-darwin = fetchFromGitHub {
    owner = "3noch"; # TODO: Update to LnL7 once PR is merged: https://github.com/LnL7/nix-darwin/pull/78
    repo = "nix-darwin";
    rev = "adfe63988d8e0f07739bc7dafd7249c3a78faf96";
    sha256 = "0rca00lajdzf8lf2hgwn6mbmii656dnw725y6nnraz4qf87907zq";
  };
  # TODO: This should probably be upstreamed to nixpkgs.
  plistLib = import (nix-darwin + /modules/launchd/lib.nix) { lib = nixpkgs.lib; };
  ios = iosWithHaskellPackages ghcIosAarch64;
  iosWithHaskellPackages = ghcIosAarch64: {
    buildApp = import ./ios {
      inherit ghcIosAarch64 plistLib;
      nixpkgs = nixpkgsFunc { system = "x86_64-darwin"; };
    };
  };
in let this = rec {
  inherit nixpkgs
          nixpkgsCross
          overrideCabal
          hackGet
          extendHaskellPackages
          foreignLibSmuggleHeaders
          ghc
          ghcHEAD
          ghc8_4_2
          ghc8_2_2
          ghc8_0_2
          ghc7
          ghcIosSimulator64
          ghcIosAarch64
          ghcIosAarch32
          ghcAndroidAarch64
          ghcAndroidAarch32
          ghcjs
          ghcjs8_2_2
          ghcjs8_4_2
          android
          androidWithHaskellPackages
          ios
          iosWithHaskellPackages
          filterGit;
  androidReflexTodomvc = android.buildApp {
    package = p: p.reflex-todomvc;
    executableName = "reflex-todomvc";
    applicationId = "org.reflexfrp.todomvc";
    displayName = "Reflex TodoMVC";
  };
  iosReflexTodomvc = ios.buildApp {
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

  attrsToList = s: map (name: { inherit name; value = builtins.getAttr name s; }) (builtins.attrNames s);
  mapSet = f: s: builtins.listToAttrs (map ({name, value}: {
    inherit name;
    value = f value;
  }) (attrsToList s));
  mkSdist = pkg: pkg.override {
    mkDerivation = drv: ghc.mkDerivation (drv // {
      postConfigure = ''
        ./Setup sdist
        mkdir "$out"
        mv dist/*.tar.gz "$out/${drv.pname}-${drv.version}.tar.gz"
        exit 0
      '';
      doHaddock = false;
    });
  };
  sdists = mapSet mkSdist ghc;
  mkHackageDocs = pkg: pkg.override {
    mkDerivation = drv: ghc.mkDerivation (drv // {
      postConfigure = ''
        ./Setup haddock --hoogle --hyperlink-source --html --for-hackage --haddock-option=--built-in-themes
        cd dist/doc/html
        mkdir "$out"
        tar cz --format=ustar -f "$out/${drv.pname}-${drv.version}-docs.tar.gz" "${drv.pname}-${drv.version}-docs"
        exit 0
      '';
      doHaddock = false;
    });
  };
  hackageDocs = mapSet mkHackageDocs ghc;
  mkReleaseCandidate = pkg: nixpkgs.stdenv.mkDerivation (rec {
    name = pkg.name + "-rc";
    sdist = mkSdist pkg + "/${pkg.pname}-${pkg.version}.tar.gz";
    docs = mkHackageDocs pkg + "/${pkg.pname}-${pkg.version}-docs.tar.gz";

    builder = builtins.toFile "builder.sh" ''
      source $stdenv/setup

      mkdir "$out"
      echo -n "${pkg.pname}-${pkg.version}" >"$out/pkgname"
      ln -s "$sdist" "$docs" "$out"
    '';

    # 'checked' isn't used, but it is here so that the build will fail if tests fail
    checked = overrideCabal pkg (drv: {
      doCheck = true;
      src = sdist;
    });
  });
  releaseCandidates = mapSet mkReleaseCandidate ghc;

  androidDevTools = [
    ghc.haven
    nixpkgs.maven
    nixpkgs.androidsdk
  ];

  # Tools that are useful for development under both ghc and ghcjs
  generalDevTools = haskellPackages:
    let nativeHaskellPackages = ghc;
    in [
    nativeHaskellPackages.Cabal
    nativeHaskellPackages.cabal-install
    nativeHaskellPackages.ghcid
    nativeHaskellPackages.hasktags
    nativeHaskellPackages.hlint
    nixpkgs.cabal2nix
    nixpkgs.curl
    nixpkgs.nix-prefetch-scripts
    nixpkgs.nodejs
    nixpkgs.pkgconfig
    nixpkgs.closurecompiler
  ] ++ (optionals (!(haskellPackages.ghc.isGhcjs or false) && builtins.compareVersions haskellPackages.ghc.version "8.2" < 0) [
    # ghc-mod doesn't currently work on ghc 8.2.2; revisit when https://github.com/DanielG/ghc-mod/pull/911 is closed
    # When ghc-mod is included in the environment without being wrapped in justStaticExecutables, it prevents ghc-pkg from seeing the libraries we install
    (nixpkgs.haskell.lib.justStaticExecutables nativeHaskellPackages.ghc-mod)
    haskellPackages.hdevtools
  ]) ++ (if builtins.compareVersions haskellPackages.ghc.version "7.10" >= 0 then [
    nativeHaskellPackages.stylish-haskell # Recent stylish-haskell only builds with AMP in place
  ] else []);

  nativeHaskellPackages = haskellPackages:
    if haskellPackages.isGhcjs or false
    then haskellPackages.ghc
    else haskellPackages;

  workOn = haskellPackages: package: (overrideCabal package (drv: {
    buildDepends = (drv.buildDepends or []) ++ generalDevTools (nativeHaskellPackages haskellPackages);
  })).env;

  workOnMulti' = { env, packageNames, tools ? _: [] }:
    let ghcEnv =
      let inherit (builtins) filter all concatLists;
          dependenciesOf = x: (x.buildDepends or [])
                           ++ (x.libraryHaskellDepends or [])
                           ++ (x.executableHaskellDepends or [])
                           ++ (x.testHaskellDepends or []);
          elemByPname = p: all (pname: (p.pname or "") != pname) packageNames;
          overiddenOut  = pkgEnv: n: (overrideCabal pkgEnv.${n} (args: {
            passthru = (args.passthru or {}) // {
              out = filter elemByPname (dependenciesOf args);
            };
          })).out;
      in env.ghc.withPackages (pkgEnv: concatLists (map (overiddenOut pkgEnv) packageNames));

    in nixpkgs.runCommand "shell" (ghcEnv.ghcEnvVars // {
      buildInputs = [
        ghcEnv
      ] ++ generalDevTools env ++ tools env;
    }) "";

  workOnMulti = env: packageNames: workOnMulti' { inherit env packageNames; };

  # A simple derivation that just creates a file with the names of all of its inputs.  If built, it will have a runtime dependency on all of the given build inputs.
  pinBuildInputs = drvName: buildInputs: otherDeps: nixpkgs.runCommand drvName {
    buildCommand = ''
      mkdir "$out"
      echo "$propagatedBuildInputs $buildInputs $nativeBuildInputs $propagatedNativeBuildInputs $otherDeps" > "$out/deps"
    '';
    inherit buildInputs otherDeps;
  } "";

  # The systems that we want to build for on the current system
  cacheTargetSystems = [
    "x86_64-linux"
    "i686-linux"
    "x86_64-darwin"
  ];

  isSuffixOf = suffix: s:
    let suffixLen = builtins.stringLength suffix;
    in builtins.substring (builtins.stringLength s - suffixLen) suffixLen s == suffix;

  reflexEnv = platform:
    let haskellPackages = builtins.getAttr platform this;
        ghcWithStuff = if platform == "ghc" || platform == "ghcjs" then haskellPackages.ghcWithHoogle else haskellPackages.ghcWithPackages;
    in ghcWithStuff (p: import ./packages.nix { haskellPackages = p; inherit platform; });

  tryReflexPackages = generalDevTools ghc
    ++ builtins.map reflexEnv platforms;

  cachePackages =
    let otherPlatforms = optionals (system == "x86_64-linux") [
#          "ghcAndroidAarch64"
#          "ghcAndroidAarch32"
        ] ++ optional iosSupport "ghcIosAarch64";
    in tryReflexPackages
      ++ builtins.map reflexEnv otherPlatforms
      ++ optionals (system == "x86_64-linux") [
#        androidDevTools
#        androidReflexTodomvc
      ] ++ optionals iosSupport [
        iosReflexTodomvc
      ];


  demoVM = (import "${nixpkgs.path}/nixos" {
    configuration = {
      imports = [
        "${nixpkgs.path}/nixos/modules/virtualisation/virtualbox-image.nix"
        "${nixpkgs.path}/nixos/modules/profiles/demo.nix"
      ];
      environment.systemPackages = tryReflexPackages;
      nixpkgs = { localSystem.system = "x86_64-linux"; };
    };
  }).config.system.build.virtualBoxOVA;

  lib = haskellLib;
  inherit cabal2nixResult system iosSupport;
  project = args: import ./project this (args ({ pkgs = nixpkgs; } // this));
  tryReflexShell = pinBuildInputs ("shell-" + system) tryReflexPackages [];
  js-framework-benchmark-src = hackGet ./js-framework-benchmark;
  ghcjsExternsJs = ./ghcjs.externs.js;
}; in this
