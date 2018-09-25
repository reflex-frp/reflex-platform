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
, nixpkgsOverlays ? []
}:
let iosSupport =
      if system != "x86_64-darwin" then false
      else if iosSupportForce || builtins.pathExists iosSdkLocation then true
      else lib.warn "No iOS sdk found at ${iosSdkLocation}; iOS support disabled.  To enable, either install a version of Xcode that provides that SDK or override the value of iosSdkVersion to match your installed version." false;
    androidSupport = lib.elem system [ "x86_64-linux" ];

    mobileGhcOverlay = import ./nixpkgs-overlays/mobile-ghc { inherit lib; };

    globalOverlays = [
      (self: super: {
        all-cabal-hashes = super.all-cabal-hashes.override {
          src-spec = {
            owner = "commercialhaskell";
            repo = "all-cabal-hashes";
            rev = "82a8a1a49240a1b465c95de6fa6bf56323ee858f";
            sha256 = "1jdzl5fyp1qcsi1anjig6kglq4jjsdll53nissjcnxpy3jscmarm";
          };
        };
      })
    ] ++ nixpkgsOverlays;

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
      overlays = globalOverlays ++ [ mobileGhcOverlay ];
      config = {
        allowUnfree = true;
        allowBroken = true; # GHCJS is marked broken in 011c149ed5e5a336c3039f0b9d4303020cff1d86
        permittedInsecurePackages = [
          "webkitgtk-2.4.11"
        ];
        packageOverrides = pkgs: {
          webkitgtk = pkgs.webkitgtk216x;
          # cabal2nix's tests crash on 32-bit linux; see https://github.com/NixOS/cabal2nix/issues/272
          ${if system == "i686-linux" then "cabal2nix" else null} = pkgs.haskell.lib.dontCheck pkgs.cabal2nix;
        };
      } // config;
    });

    inherit (nixpkgs) lib fetchurl fetchgit fetchgitPrivate fetchFromGitHub;

    nixpkgsCross = {
      android = lib.mapAttrs (_: args: if args == null then null else nixpkgsFunc args) rec {
        aarch64 = {
          system = "x86_64-linux";
          overlays = globalOverlays ++ [ mobileGhcOverlay ];
          crossSystem = {
            config = "aarch64-unknown-linux-android";
            arch = "arm64";
            libc = "bionic";
            withTLS = true;
            openssl.system = "linux-generic64";
            platform = lib.systems.examples.aarch64-multiplatform;
            useAndroidPrebuilt = true;
          };
          config.allowUnfree = true;
        };
        aarch32 = {
          system = "x86_64-linux";
          overlays = globalOverlays ++ [ mobileGhcOverlay ];
          crossSystem = {
            config = "arm-unknown-linux-androideabi";
            arch = "armv7";
            libc = "bionic";
            withTLS = true;
            openssl.system = "linux-generic32";
            platform = lib.systems.exmamples.armv7l-hf-multiplatform;
            useAndroidPrebuilt = true;
          };
          config.allowUnfree = true;
        };
        # Back compat
        arm64 = lib.warn "nixpkgsCross.android.arm64 has been deprecated, using nixpkgsCross.android.aarch64 instead." aarch64;
        armv7a = lib.warn "nixpkgsCross.android.armv7a has been deprecated, using nixpkgsCross.android.aarch32 instead." aarch32;
        arm64Impure = lib.warn "nixpkgsCross.android.arm64Impure has been deprecated, using nixpkgsCross.android.aarch64 instead." aarch64;
        armv7aImpure = lib.warn "nixpkgsCross.android.armv7aImpure has been deprecated, using nixpkgsCross.android.aarch32 instead." aarch32;
      };
      ios =
        let config = {
              allowUnfree = true;
              packageOverrides = p: {
                darwin = p.darwin // {
                  ios-cross = p.darwin.ios-cross.override {
                    # Depending on where ghcHEAD is in your nixpkgs checkout, you may need llvm 39 here instead
                    inherit (p.llvmPackages_39) llvm clang;
                  };
                };
                buildPackages = p.buildPackages // {
                  osx_sdk = p.buildPackages.callPackage ({ stdenv }:
                    let version = "10";
                    in stdenv.mkDerivation rec {
                    name = "iOS.sdk";

                    src = p.stdenv.cc.sdk;

                    unpackPhase    = "true";
                    configurePhase = "true";
                    buildPhase     = "true";
                    target_prefix = stdenv.lib.replaceStrings ["-"] ["_"] p.targetPlatform.config;
                    setupHook = ./scripts/setup-hook-ios.sh;

                    installPhase = ''
                      mkdir -p $out/
                      echo "Source is: $src"
                      cp -r $src/* $out/
                    '';

                    meta = with stdenv.lib; {
                      description = "The IOS OS ${version} SDK";
                      maintainers = with maintainers; [ copumpkin ];
                      platforms   = platforms.darwin;
                      license     = licenses.unfree;
                    };
                  }) {};
                };
              };
            };
        in lib.mapAttrs (_: args: if args == null then null else nixpkgsFunc args) rec {
        simulator64 = {
          system = "x86_64-darwin";
          overlays = globalOverlays ++ [ mobileGhcOverlay appleLibiconvHack ];
          crossSystem = {
            useIosPrebuilt = true;
            # You can change config/arch/isiPhoneSimulator depending on your target:
            # aarch64-apple-darwin14 | arm64  | false
            # arm-apple-darwin10     | armv7  | false
            # i386-apple-darwin11    | i386   | true
            # x86_64-apple-darwin14  | x86_64 | true
            config = "x86_64-apple-darwin14";
            arch = "x86_64";
            isiPhoneSimulator = true;
            sdkVer = iosSdkVersion;
            useiOSCross = true;
            openssl.system = "darwin64-x86_64-cc";
            libc = "libSystem";
          };
          inherit config;
        };
        aarch64 = {
          system = "x86_64-darwin";
          overlays = globalOverlays ++ [ mobileGhcOverlay appleLibiconvHack ];
          crossSystem = {
            useIosPrebuilt = true;
            # You can change config/arch/isiPhoneSimulator depending on your target:
            # aarch64-apple-darwin14 | arm64  | false
            # arm-apple-darwin10     | armv7  | false
            # i386-apple-darwin11    | i386   | true
            # x86_64-apple-darwin14  | x86_64 | true
            config = "aarch64-apple-darwin14";
            arch = "arm64";
            isiPhoneSimulator = false;
            sdkVer = iosSdkVersion;
            useiOSCross = true;
            openssl.system = "ios64-cross";
            libc = "libSystem";
          };
          inherit config;
        };
        # Back compat
        arm64 = lib.warn "nixpkgsCross.ios.arm64 has been deprecated, using nixpkgsCross.ios.aarch64 instead." aarch64;
      };
    };

    haskellLib = nixpkgs.haskell.lib;

    filterGit = builtins.filterSource (path: type: !(builtins.any (x: x == baseNameOf path) [".git" "tags" "TAGS" "dist"]));

    # Retrieve source that is controlled by the hack-* scripts; it may be either a stub or a checked-out git repo
    hackGet = p:
      if builtins.pathExists (p + "/git.json") then (
        let gitArgs = builtins.fromJSON (builtins.readFile (p + "/git.json"));
        in if builtins.elem "@" (lib.stringToCharacters gitArgs.url)
        then fetchgitPrivate gitArgs
        else fetchgit gitArgs)
      else if builtins.pathExists (p + "/github.json") then fetchFromGitHub (builtins.fromJSON (builtins.readFile (p + "/github.json")))
      else {
        name = baseNameOf p;
        outPath = filterGit p;
      };

    # All imports of sources need to go here, so that they can be explicitly cached
    sources = {
      ghcjs-boot = hackGet ./ghcjs-boot;
      shims = hackGet ./shims;
      ghcjs = hackGet ./ghcjs;
    };

    inherit (nixpkgs.stdenv.lib) optional optionals optionalAttrs;

    optionalExtension = cond: overlay: if cond then overlay else _: _: {};

in with lib; with haskellLib;

let overrideCabal = pkg: f: if pkg == null then null else haskellLib.overrideCabal pkg f;

    replaceSrc = pkg: src: version: overrideCabal pkg (drv: {
      inherit src version;
      sha256 = null;
      revision = null;
      editedCabalFile = null;
    });

    combineOverrides = old: new: old // new // optionalAttrs (old ? overrides && new ? overrides) {
      overrides = lib.composeExtensions old.overrides new.overrides;
    };

    # Makes sure that old `overrides` from a previous call to `override` are not
    # forgotten, but composed. Do this by overriding `override` and passing a
    # function which takes the old argument set and combining it. What a tongue
    # twister!
    makeRecursivelyOverridable = x: x // {
      override = new: makeRecursivelyOverridable (x.override (old: (combineOverrides old new)));
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

    mkHaskellOverlays = nixpkgs: import ./haskell-overlays {
      inherit
        haskellLib
        nixpkgs fetchFromGitHub hackGet
        useFastWeak useReflexOptimizer enableLibraryProfiling enableTraceReflexEvents
        stage2Script;
      inherit (nixpkgs) lib;
      androidActivity = hackGet ./android-activity;
    };

    stage2Script = nixpkgs.runCommand "stage2.nix" {
      GEN_STAGE2 = builtins.readFile (nixpkgs.path + "/pkgs/development/compilers/ghcjs/gen-stage2.rb");
      buildCommand = ''
        echo "$GEN_STAGE2" > gen-stage2.rb && chmod +x gen-stage2.rb
        patchShebangs .
        ./gen-stage2.rb "${sources.ghcjs-boot}" >"$out"
      '';
      nativeBuildInputs = with nixpkgs; [
        ruby cabal2nix
      ];
    } "";

    ghcjsCompiler = ghc.callPackage (nixpkgs.path + "/pkgs/development/compilers/ghcjs/base.nix") {
      bootPkgs = ghc;
      ghcjsSrc = sources.ghcjs;
      ghcjsBootSrc = sources.ghcjs-boot;
      shims = sources.shims;
      stage2 = import stage2Script;
    };

    ghcjsPackages = nixpkgs.callPackage (nixpkgs.path + "/pkgs/development/haskell-modules") {
      ghc = ghcjsCompiler;
      compilerConfig = nixpkgs.callPackage (nixpkgs.path + "/pkgs/development/haskell-modules/configuration-ghc-8.0.x.nix") { inherit haskellLib; };
      packageSetConfig = nixpkgs.callPackage (nixpkgs.path + "/pkgs/development/haskell-modules/configuration-ghcjs.nix") { inherit haskellLib; };
      inherit haskellLib;
    };

#    TODO: Figure out why this approach doesn't work; it doesn't seem to evaluate our overridden ghc at all
#    ghcjsPackages = nixpkgs.haskell.packages.ghcjs.override {
#      ghc = builtins.trace "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" ghcjsCompiler;
#    };
  ghcjs = (makeRecursivelyOverridable ghcjsPackages).override {
    overrides = lib.foldr lib.composeExtensions (_: _: {}) (let
      haskellOverlays = mkHaskellOverlays nixpkgs;
    in [
      haskellOverlays.reflexPackages
      haskellOverlays.untriaged
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      haskellOverlays.ghcjs
      (optionalExtension useTextJSString haskellOverlays.textJSString)
    ]);
  };
  ghcHEAD = (makeRecursivelyOverridable nixpkgs.haskell.packages.ghcHEAD).override {
    overrides = lib.foldr lib.composeExtensions (_: _: {}) (let
      haskellOverlays = mkHaskellOverlays nixpkgs;
    in [
      haskellOverlays.reflexPackages
      haskellOverlays.untriaged
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      haskellOverlays.ghc-head
    ]);
  };
  ghc8_2 = ghc8_2_1;
  ghc8_2_1 = (makeRecursivelyOverridable nixpkgs.haskell.packages.ghc821).override {
    overrides = lib.foldr lib.composeExtensions (_: _: {}) (let
      haskellOverlays = mkHaskellOverlays nixpkgs;
    in [
      haskellOverlays.reflexPackages
      haskellOverlays.untriaged
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      haskellOverlays.ghc-8_2
    ]);
  };
  ghc = ghc8_0;
  ghc8_0 = (makeRecursivelyOverridable nixpkgs.haskell.packages.ghc802).override {
    overrides = lib.foldr lib.composeExtensions (_: _: {}) (let
      haskellOverlays = mkHaskellOverlays nixpkgs;
    in [
      haskellOverlays.reflexPackages
      haskellOverlays.untriaged
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      haskellOverlays.ghc-8
    ]);
  };
  ghc7 = (makeRecursivelyOverridable nixpkgs.haskell.packages.ghc7103).override {
    overrides = lib.foldr lib.composeExtensions (_: _: {}) (let
      haskellOverlays = mkHaskellOverlays nixpkgs;
    in [
      haskellOverlays.reflexPackages
      haskellOverlays.untriaged
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      haskellOverlays.ghc-7
    ]);
  };
  ghc7_8 = (makeRecursivelyOverridable nixpkgs.haskell.packages.ghc784).override {
    overrides = lib.foldr lib.composeExtensions (_: _: {}) (let
      haskellOverlays = mkHaskellOverlays nixpkgs;
    in [
      haskellOverlays.reflexPackages
      haskellOverlays.untriaged
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      haskellOverlays.ghc-7_8
    ]);
  };
  ghcAndroidAarch64 = (makeRecursivelyOverridable nixpkgsCross.android.aarch64.haskell.packages.ghc821).override {
    overrides = lib.foldr lib.composeExtensions (_: _: {}) (let
      haskellOverlays = mkHaskellOverlays nixpkgsCross.android.aarch64;
    in [
      haskellOverlays.reflexPackages
      haskellOverlays.untriaged
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      haskellOverlays.ghc-8_2
      haskellOverlays.disableTemplateHaskell
      haskellOverlays.android
    ]);
  };
  ghcAndroidAarch32 = (makeRecursivelyOverridable nixpkgsCross.android.aarch32.haskell.packages.ghc821).override {
    overrides = lib.foldr lib.composeExtensions (_: _: {}) (let
      haskellOverlays = mkHaskellOverlays nixpkgsCross.android.aarch32;
    in [
      haskellOverlays.reflexPackages
      haskellOverlays.untriaged
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      haskellOverlays.ghc-8_2
      haskellOverlays.disableTemplateHaskell
      haskellOverlays.android
    ]);
  };
  ghcIosSimulator64 = (makeRecursivelyOverridable nixpkgsCross.ios.simulator64.haskell.packages.ghc821).override {
    overrides = lib.foldr lib.composeExtensions (_: _: {}) (let
      haskellOverlays = mkHaskellOverlays nixpkgsCross.ios.simulator64;
    in [
      haskellOverlays.reflexPackages
      haskellOverlays.untriaged
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      haskellOverlays.ghc-8_2
    ]);
  };
  ghcIosAarch64 = (makeRecursivelyOverridable nixpkgsCross.ios.aarch64.haskell.packages.ghc821).override {
    overrides = lib.foldr lib.composeExtensions (_: _: {}) (let
      haskellOverlays = mkHaskellOverlays nixpkgsCross.ios.aarch64;
    in [
      haskellOverlays.reflexPackages
      haskellOverlays.untriaged
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      haskellOverlays.ghc-8_2
      haskellOverlays.disableTemplateHaskell
      haskellOverlays.ios
    ]);
  };
  ghcIosAarch32 = (makeRecursivelyOverridable nixpkgsCross.ios.aarch32.haskell.packages.ghc821).override {
    overrides = lib.foldr lib.composeExtensions (_: _: {}) (let
      haskellOverlays = mkHaskellOverlays nixpkgsCross.ios.aarch32;
    in [
      haskellOverlays.reflexPackages
      haskellOverlays.untriaged
      (optionalExtension enableExposeAllUnfoldings haskellOverlays.exposeAllUnfoldings)
      haskellOverlays.ghc-8_2
      haskellOverlays.disableTemplateHaskell
      haskellOverlays.ios
    ]);
  };
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
  plistLib = import (nix-darwin + /modules/launchd/lib.nix) { inherit lib; };

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
          foreignLibSmuggleHeaders
          stage2Script
          ghc
          ghcHEAD
          ghc8_2
          ghc8_0
          ghc7
          ghc7_8
          ghcIosSimulator64
          ghcIosAarch64
          ghcIosAarch32
          ghcAndroidAarch64
          ghcAndroidAarch32
          ghcjs
          android
          androidWithHaskellPackages
          ios
          iosWithHaskellPackages
          filterGit;

  # Back compat
  ghcAndroidArm64 = lib.warn "ghcAndroidArm64 has been deprecated, using ghcAndroidAarch64 instead." ghcAndroidAarch64;
  ghcAndroidArmv7a = lib.warn "ghcAndroidArmv7a has been deprecated, using ghcAndroidAarch32 instead." ghcAndroidAarch32;
  ghcIosArm64 = lib.warn "ghcIosArm64 has been deprecated, using ghcIosAarch64 instead." ghcIosAarch64;

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
        concatCombinableAttrs = haskellConfigs: filterAttrs (n: v: v != []) (listToAttrs (map (name: { inherit name; value = concatLists (map (haskellConfig: haskellConfig.${name} or []) haskellConfigs); }) combinableAttrs));
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

  # A simple derivation that just creates a file with the names of all of its inputs.  If built, it will have a runtime dependency on all of the given build inputs.
  pinBuildInputs = drvName: buildInputs: otherDeps: nixpkgs.runCommand drvName {
    buildCommand = ''
      mkdir "$out"
      echo "$propagatedBuildInputs $buildInputs $nativeBuildInputs $propagatedNativeBuildInputs $otherDeps" > "$out/deps"
    '';
    inherit buildInputs otherDeps;
  } "";

  # The systems that we want to build for on the current system
  cacheTargetSystems = lib.warn "cacheTargetSystems has been deprecated, use cacheBuildSystems" cacheBuildSystems;
  cacheBuildSystems = [
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
    let otherPlatforms = optionals androidSupport [
          "ghcAndroidAarch64"
          "ghcAndroidAarch32"
        ] ++ optional iosSupport "ghcIosAarch64";
    in tryReflexPackages
      ++ builtins.map reflexEnv otherPlatforms
      ++ optionals androidSupport [
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
    };
  }).config.system.build.virtualBoxOVA;

  inherit cabal2nixResult sources system androidSupport iosSupport;
  project = args: import ./project this (args ({ pkgs = nixpkgs; } // this));
  tryReflexShell = pinBuildInputs ("shell-" + system) tryReflexPackages [];
  js-framework-benchmark-src = hackGet ./js-framework-benchmark;
  ghcjsExternsJs = ./ghcjs.externs.js;
}; in this
