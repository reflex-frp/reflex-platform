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
, nixpkgsOverlays ? []
}:
let iosSupport = system != "x86_64-darwin";
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
    androidPICPatches = self: super: {
      haskell = super.haskell // {
        compiler = super.haskell.compiler // {
          integer-simple = super.haskell.compiler.integer-simple // {
            ghc843 = super.haskell.compiler.integer-simple.ghc843
                     .overrideAttrs (drv: {
              patches = (drv.patches or [])
                      ++ [ ./android/patches/force-relocation.patch ];
            });
          };
        };
      };
    };
    nixpkgsArgs = {
      config = {
        permittedInsecurePackages = [
          "webkitgtk-2.4.11"
        ];
        packageOverrides = pkgs: {
          webkitgtk = pkgs.webkitgtk220x;
          # cabal2nix's tests crash on 32-bit linux; see https://github.com/NixOS/cabal2nix/issues/272
          ${if system == "i686-linux" then "cabal2nix" else null} = pkgs.haskell.lib.dontCheck pkgs.cabal2nix;
        };

        # XCode needed for native macOS app
        # Obelisk needs it to for some reason
        allowUnfree = true;
      } // config;
      overlays = nixpkgsOverlays;
    };
    nixpkgs = nixpkgsFunc (nixpkgsArgs // { inherit system; });
    inherit (nixpkgs) fetchurl fetchgit fetchgitPrivate fetchFromGitHub;
    nixpkgsCross = {
      android = nixpkgs.lib.mapAttrs (_: args: nixpkgsFunc (nixpkgsArgs // args)) rec {
        aarch64 = {
          system = "x86_64-linux";
          overlays = nixpkgsArgs.overlays ++ [androidPICPatches];
          crossSystem = nixpkgs.lib.systems.examples.aarch64-android-prebuilt;
        };
        aarch32 = {
          system = "x86_64-linux";
          overlays = nixpkgsArgs.overlays ++ [androidPICPatches];
          crossSystem = nixpkgs.lib.systems.examples.armv7a-android-prebuilt;
        };
        # Back compat
        arm64Impure = builtins.trace "Warning: nixpkgsCross.android.arm64Impure has been deprecated, using nixpkgsCross.android.aarch64 instead." aarch64;
        armv7aImpure = builtins.trace "Warning: nixpkgsCross.android.armv7aImpure has been deprecated, using nixpkgsCross.android.aarch32 instead." aarch32;
      };
      ios = nixpkgs.lib.mapAttrs (_: args: nixpkgsFunc (nixpkgsArgs // args)) rec {
        simulator64 = {
          system = "x86_64-darwin";
          overlays = nixpkgsArgs.overlays ++ [appleLibiconvHack];
          crossSystem = nixpkgs.lib.systems.examples.iphone64-simulator // {
            sdkVer = iosSdkVersion;
          };
        };
        aarch64 = {
          system = "x86_64-darwin";
          overlays = nixpkgsArgs.overlays ++ [appleLibiconvHack];
          crossSystem = nixpkgs.lib.systems.examples.iphone64 // {
            sdkVer = iosSdkVersion;
          };
        };
        aarch32 = {
          system = "x86_64-darwin";
          overlays = nixpkgsArgs.overlays ++ [appleLibiconvHack];
          crossSystem = nixpkgs.lib.systems.examples.iphone32 // {
            sdkVer = iosSdkVersion;
          };
        };
        # Back compat
        arm64 = builtins.trace "Warning: nixpkgsCross.ios.arm64 has been deprecated, using nixpkgsCross.ios.aarch64 instead." aarch64;
      };
    };
    haskellLib = nixpkgs.haskell.lib;
    filterGit = builtins.filterSource (path: type: !(builtins.any (x: x == baseNameOf path) [".git" "tags" "TAGS" "dist"]));
    # Retrieve source that is controlled by the hack-* scripts; it may be either a stub or a checked-out git repo
    hackGet = p:
      let filterArgs = x: removeAttrs x [ "branch" ];
      in if builtins.pathExists (p + "/git.json") then (
        let gitArgs = filterArgs (builtins.fromJSON (builtins.readFile (p + "/git.json")));
        in if builtins.elem "@" (nixpkgs.lib.stringToCharacters gitArgs.url)
        then fetchgitPrivate gitArgs
        else fetchgit gitArgs)
      else if builtins.pathExists (p + "/github.json") then fetchFromGitHub (filterArgs (builtins.fromJSON (builtins.readFile (p + "/github.json"))))
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
in with nixpkgs.lib; with haskellLib;
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
        patches = (o.patches or [])
                ++ optional useFastWeak ./fast-weak.patch;
        phases = [ "unpackPhase" "patchPhase" "buildPhase" ];
      });
    };

    extendHaskellPackages = haskellPackages: makeRecursivelyOverridable haskellPackages {
      overrides = self: super:
        let reflexDom = import (hackGet ./reflex-dom) self nixpkgs;
            jsaddlePkgs = import (hackGet ./jsaddle) self;
            gargoylePkgs = self.callPackage (hackGet ./gargoyle) self;
            ghcjsDom = import (hackGet ./ghcjs-dom) self;
            addReflexOptimizerFlag = if useReflexOptimizer
              then drv: appendConfigureFlag drv "-fuse-reflex-optimizer"
              else drv: drv;
        in {

        ########################################################################
        # Reflex packages
        ########################################################################
        reflex = dontCheck (addFastWeakFlag (addReflexTraceEventsFlag (addReflexOptimizerFlag (self.callPackage (hackGet ./reflex) {}))));
        reflex-todomvc = self.callPackage (hackGet ./reflex-todomvc) {};
        reflex-aeson-orphans = self.callCabal2nix "reflex-aeson-orphans" (hackGet ./reflex-aeson-orphans) {};

        # Broken Haddock - Please fix!
        # : error is: haddock: internal error: internal: extractDecl
        # No idea where it hits?
        reflex-dom = dontHaddock (addReflexOptimizerFlag reflexDom.reflex-dom);
        reflex-dom-core = dontHaddock (addReflexOptimizerFlag reflexDom.reflex-dom-core);

        inherit (jsaddlePkgs) jsaddle jsaddle-clib
                              jsaddle-webkit2gtk jsaddle-webkitgtk
                              jsaddle-wkwebview;

        # another broken test
        # phantomjs has issues with finding the right port
        # jsaddle-warp = dontCheck (addTestToolDepend jsaddlePkgs.jsaddle-warp nixpkgs.phantomjs);
        jsaddle-warp = dontCheck jsaddlePkgs.jsaddle-warp;

        jsaddle-dom = self.callPackage (hackGet ./jsaddle-dom) {};

        haskell-gi-overloading = dontHaddock (self.callHackage "haskell-gi-overloading" "0.0" {});

        inherit (ghcjsDom) ghcjs-dom-jsffi;

        inherit (gargoylePkgs) gargoyle gargoyle-postgresql;

        language-nix = dontCheck super.language-nix;
        hasktags = dontCheck super.hasktags;
        http-reverse-proxy = dontCheck super.http-reverse-proxy;
        xmlhtml = dontCheck super.xmlhtml;
        haven = doJailbreak super.haven;
        mmorph = doJailbreak super.mmorph;
        async = self.callHackage "async" "2.2.1" {};
        lifted-async = self.callHackage "lifted-async" "0.10.0.2" {};
        hinotify = self.callHackage "hinotify" "0.3.10" {};
        fsnotify = self.callHackage "fsnotify" "0.3.0.1" {};

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

        mkDerivation = expr: super.mkDerivation (expr // {
          inherit enableLibraryProfiling;
        });
      };
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
    ghcjs8_2Packages = nixpkgs.callPackage (nixpkgs.path + "/pkgs/development/haskell-modules") {
      ghc = ghc8_2.ghcjs;
      buildHaskellPackages = ghc8_2.ghcjs.bootPkgs;
      compilerConfig = nixpkgs.callPackage (nixpkgs.path + "/pkgs/development/haskell-modules/configuration-ghc-8.2.x.nix") { inherit haskellLib; };
      packageSetConfig = nixpkgs.callPackage (nixpkgs.path + "/pkgs/development/haskell-modules/configuration-ghcjs.nix") { inherit haskellLib; };
      inherit haskellLib;
    };
    ghcjs8_4Packages = nixpkgs.callPackage (nixpkgs.path + "/pkgs/development/haskell-modules") {
      ghc = ghc8_4.ghcjs;
      buildHaskellPackages = ghc8_4.ghcjs.bootPkgs;
      compilerConfig = nixpkgs.callPackage (nixpkgs.path + "/pkgs/development/haskell-modules/configuration-ghc-8.4.x.nix") { inherit haskellLib; };
      packageSetConfig = nixpkgs.callPackage (nixpkgs.path + "/pkgs/development/haskell-modules/configuration-ghcjs.nix") { inherit haskellLib; };
      inherit haskellLib;
    };
  ghc = ghc8_4;
  ghcjs8_2 = (extendHaskellPackages ghcjs8_2Packages).override {
    overrides = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions (_: _: {}) [
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      haskellOverlays.ghcjs
      (optionalExtension useTextJSString haskellOverlays.textJSString)
    ];
  };
  ghcjs8_4 = (extendHaskellPackages ghcjs8_4Packages).override {
    overrides = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions (_: _: {}) [
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      haskellOverlays.ghcjs
      (optionalExtension useTextJSString haskellOverlays.textJSString)
    ];
  };
  ghcjs = ghcjs8_4;
  ghcHEAD = (extendHaskellPackages nixpkgs.pkgs.haskell.packages.ghcHEAD).override {
    overrides = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions (_: _: {}) [
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      haskellOverlays.ghc-head
    ];
  };
  ghc8_4 = (extendHaskellPackages nixpkgs.pkgs.haskell.packages.ghc843).override {
    overrides = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions (_: _: {}) [
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      (ghcjsPkgs (nixpkgs.pkgs.haskell.compiler.ghcjs84.override {
        ghcjsSrc = fetchgit {
          url = "https://github.com/obsidiansystems/ghcjs.git";
          rev = "e2391d275aa3bc5159e269accd9816882e29db9b";
          sha256 = "0yn1cf6c9c2gc1z49d2ywqilrg97sf00f4zp3fp3l6xj399nc309";
          fetchSubmodules = true;
        };
      }))
      haskellOverlays.ghc-8_4
    ];
  };
  ghc8_2 = (extendHaskellPackages nixpkgs.pkgs.haskell.packages.ghc822).override {
    overrides = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions (_: _: {}) [
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      (ghcjsPkgs nixpkgs.pkgs.haskell.compiler.ghcjs82)
      haskellOverlays.ghc-8_2
    ];
  };
  ghc8_0 = (extendHaskellPackages nixpkgs.pkgs.haskell.packages.ghc802).override {
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
  ghcAndroidAarch64 = (extendHaskellPackages nixpkgsCross.android.aarch64.pkgs.haskell.packages.integer-simple.ghc843).override {
    overrides = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions (_: _: {}) [
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      haskellOverlays.ghc-8_4
      haskellOverlays.disableTemplateHaskell
      haskellOverlays.android
    ];
  };
  ghcAndroidAarch32 = (extendHaskellPackages nixpkgsCross.android.aarch32.pkgs.haskell.packages.integer-simple.ghc843).override {
    overrides = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions (_: _: {}) [
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      haskellOverlays.ghc-8_4
      haskellOverlays.disableTemplateHaskell
      haskellOverlays.android
    ];
  };
  ghcIosSimulator64 = (extendHaskellPackages nixpkgsCross.ios.simulator64.pkgs.haskell.packages.integer-simple.ghc843).override {
    overrides = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions (_: _: {}) [
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      haskellOverlays.disableTemplateHaskell
      haskellOverlays.ghc-8_4
      haskellOverlays.ios
    ];
  };
  ghcIosAarch64 = (extendHaskellPackages nixpkgsCross.ios.aarch64.pkgs.haskell.packages.integer-simple.ghc843).override {
    overrides = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions (_: _: {}) [
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      haskellOverlays.ghc-8_4
      haskellOverlays.disableTemplateHaskell
      haskellOverlays.ios
    ];
  };
  ghcIosAarch32 = (extendHaskellPackages nixpkgsCross.ios.aarch32.pkgs.haskell.packages.integer-simple.ghc843).override {
    overrides = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions (_: _: {}) [
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      haskellOverlays.ghc-8_4
      haskellOverlays.disableTemplateHaskell
      haskellOverlays.ios
    ];
  };
  #TODO: Separate debug and release APKs
  #TODO: Warn the user that the android app name can't include dashes
  android = androidWithHaskellPackages { inherit ghcAndroidAarch64 ghcAndroidAarch32; };
  androidWithHaskellPackages = { ghcAndroidAarch64, ghcAndroidAarch32 }: import ./android {
    nixpkgs = nixpkgsFunc (nixpkgsArgs // { system = "x86_64-linux"; });
    hostPkgs = nixpkgs;
    inherit nixpkgsCross ghcAndroidAarch64 ghcAndroidAarch32 overrideCabal;
  };
  ios = iosWithHaskellPackages ghcIosAarch64;
  iosWithHaskellPackages = ghcIosAarch64: {
    buildApp = import ./ios {
      inherit ghcIosAarch64;
      nixpkgs = nixpkgsFunc (nixpkgsArgs // { system = "x86_64-darwin"; });
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
          ghc8_4
          ghc8_2
          ghc8_0
          ghc7
          ghcIosSimulator64
          ghcIosAarch64
          ghcIosAarch32
          ghcAndroidAarch64
          ghcAndroidAarch32
          ghcjs
          ghcjs8_2
          ghcjs8_4
          android
          androidWithHaskellPackages
          ios
          iosWithHaskellPackages
          filterGit;

  # Back compat
  ghcAndroidArm64 = builtins.trace "Warning: ghcAndroidArm64 has been deprecated, using ghcAndroidAarch64 instead." ghcAndroidAarch64;
  ghcAndroidArmv7a = builtins.trace "Warning: ghcAndroidArmv7a has been deprecated, using ghcAndroidAarch32 instead." ghcAndroidAarch32;
  ghcIosArm64 = builtins.trace "Warning: ghcIosArm64 has been deprecated, using ghcIosAarch64 instead." ghcIosAarch64;

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
  mkSdist = pkg: pkg.override (oldArgs: {
    mkDerivation = drv: oldArgs.mkDerivation (drv // {
      postConfigure = ''
        ./Setup sdist
        mkdir "$out"
        mv dist/*.tar.gz "$out/${drv.pname}-${drv.version}.tar.gz"
        exit 0
      '';
      doHaddock = false;
    });
  });
  sdists = mapSet mkSdist ghc;
  mkHackageDocs = pkg: pkg.override (oldArgs: {
    mkDerivation = drv: oldArgs.mkDerivation (drv // {
      postConfigure = ''
        ./Setup haddock --hoogle --hyperlink-source --html --for-hackage --haddock-option=--built-in-themes
        cd dist/doc/html
        mkdir "$out"
        tar cz --format=ustar -f "$out/${drv.pname}-${drv.version}-docs.tar.gz" "${drv.pname}-${drv.version}-docs"
        exit 0
      '';
      doHaddock = false;
    });
  });
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

    # 'checked' isn't used, but it is here so that the build will fail
    # if tests fail
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
  generalDevToolsAttrs = haskellPackages:
    let nativeHaskellPackages = ghc;
    in {
    inherit (nativeHaskellPackages)
      Cabal
      cabal-install
      ghcid
      hasktags
      hlint;
    inherit (nixpkgs)
      cabal2nix
      curl
      nix-prefetch-scripts
      nodejs
      pkgconfig
      closurecompiler;
  } // (optionalAttrs (!(haskellPackages.ghc.isGhcjs or false) && builtins.compareVersions haskellPackages.ghc.version "8.2" < 0) {
    # ghc-mod doesn't currently work on ghc 8.2.2; revisit when https://github.com/DanielG/ghc-mod/pull/911 is closed
    # When ghc-mod is included in the environment without being wrapped in justStaticExecutables, it prevents ghc-pkg from seeing the libraries we install
    ghc-mod = (nixpkgs.haskell.lib.justStaticExecutables nativeHaskellPackages.ghc-mod);
    inherit (haskellPackages) hdevtools;
  }) // (optionalAttrs (builtins.compareVersions haskellPackages.ghc.version "7.10" >= 0) {
    inherit (nativeHaskellPackages) stylish-haskell; # Recent stylish-haskell only builds with AMP in place
  });

  generalDevTools = haskellPackages: builtins.attrValues (generalDevToolsAttrs haskellPackages);

  nativeHaskellPackages = haskellPackages:
    if haskellPackages.isGhcjs or false
    then haskellPackages.ghc
    else haskellPackages;

  workOn = haskellPackages: package: (overrideCabal package (drv: {
    buildDepends = (drv.buildDepends or []) ++ generalDevTools (nativeHaskellPackages haskellPackages);
  })).env;

  workOnMulti' = { env, packageNames, tools ? _: [], shellToolOverrides ? _: _: {} }:
    let inherit (builtins) listToAttrs filter attrValues all concatLists;
        combinableAttrs = [
          "benchmarkDepends"
          "benchmarkFrameworkDepends"
          "benchmarkHaskellDepends"
          "benchmarkPkgconfigDepends"
          "benchmarkSystemDepends"
          "benchmarkToolDepends"
          "buildDepends"
          "buildTools"
          "executableFrameworkDepends"
          "executableHaskellDepends"
          "executablePkgconfigDepends"
          "executableSystemDepends"
          "executableToolDepends"
          "extraLibraries"
          "libraryFrameworkDepends"
          "libraryHaskellDepends"
          "libraryPkgconfigDepends"
          "librarySystemDepends"
          "libraryToolDepends"
          "pkgconfigDepends"
          "setupHaskellDepends"
          "testDepends"
          "testFrameworkDepends"
          "testHaskellDepends"
          "testPkgconfigDepends"
          "testSystemDepends"
          "testToolDepends"
        ];
        concatCombinableAttrs = haskellConfigs: listToAttrs (map (name: { inherit name; value = concatLists (map (haskellConfig: haskellConfig.${name} or []) haskellConfigs); }) combinableAttrs);
        getHaskellConfig = p: (overrideCabal p (args: {
          passthru = (args.passthru or {}) // {
            out = args;
          };
        })).out;
        notInTargetPackageSet = p: all (pname: (p.pname or "") != pname) packageNames;
        baseTools = generalDevToolsAttrs env;
        overriddenTools = attrValues (baseTools // shellToolOverrides env baseTools);
        depAttrs = mapAttrs (_: v: filter notInTargetPackageSet v) (concatCombinableAttrs (concatLists [
          (map getHaskellConfig (attrVals packageNames env))
          [{
            buildTools = overriddenTools ++ tools env;
          }]
        ]));

    in (env.mkDerivation (depAttrs // {
      pname = "work-on-multi--combined-pkg";
      version = "0";
      license = null;
    })).env;

  workOnMulti = env: packageNames: workOnMulti' { inherit env packageNames; };

  # A simple derivation that just creates a file with the names of all
  # of its inputs. If built, it will have a runtime dependency on all
  # of the given build inputs.
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
    # "i686-linux"
    "x86_64-darwin"
  ];

  isSuffixOf = suffix: s:
    let suffixLen = builtins.stringLength suffix;
    in builtins.substring (builtins.stringLength s - suffixLen) suffixLen s == suffix;

  reflexEnv = platform:
    let haskellPackages = builtins.getAttr platform this;
        ghcWithStuff = if platform == "ghc" || platform == "ghcjs"
                       then haskellPackages.ghcWithHoogle
                       else haskellPackages.ghcWithPackages;
    in ghcWithStuff (p: import ./packages.nix {
      haskellPackages = p;
      inherit platform;
    });

  tryReflexPackages = generalDevTools ghc
    ++ builtins.map reflexEnv platforms;

  cachePackages =
    let otherPlatforms = optionals (system == "x86_64-linux") [
          "ghcAndroidAarch64"
          "ghcAndroidAarch32"
        ] ++ optional iosSupport "ghcIosAarch64";
    in tryReflexPackages
      ++ builtins.map reflexEnv otherPlatforms
      ++ optionals (system == "x86_64-linux") [
        androidDevTools
        androidReflexTodomvc
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
