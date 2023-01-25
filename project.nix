# NOTE: Define the interface
{ name # Name of the current project
, compiler-nix-name ? "ghc8107" # What compiler we should be using
, src # Source of the current project
, overrides ? [ ] # Overrides to packages
, extraSrcFiles ? [ ] # ExtraSrcFiles to include in the project builds
, setupCross ? true # Setup cross-compiling
, hackageOverlays ? [ ] # Overlays for hackage, to pass to the cabal solver
, allowUnfree ? false # Allow Unfree
, android_sdk_accept_license ? false # Accept android sdk license terms
, nixpkgsArgs ? { } # Extra nixpkgs arguments
, dontSplice ? [ ] # Packages to not splice
, dontHarden ? [ ] # Packages to not harden
, hardeningOpts ? [ "-fPIC" "-pie" ]
}:
let
  # TODO:
  # - Remove this let box properly
  # - Allow for pkgs to be overriden

  # Logic to bootstrap packages that isn't our local checkout
  haskell-nix = import ./submodules/haskell.nix { };
  pkgs-pre = import haskell-nix.sources.nixpkgs-unstable (haskell-nix.nixpkgsArgs);

  # Patch the packages with some commits external to our specific checkout
  remotePatches = [
    {
      url = "https://github.com/obsidiansystems/nixpkgs/commit/d39ee6b7c45deb224d95f717bd1e6e2144e09dd9.diff";
      sha256 = "sha256-stn4C43O5M0Qk80gj7YK/87qCDflnm/AwYcOXv5fErI=";
    }
    {
      url = "https://github.com/obsidiansystems/nixpkgs/commit/4516c1a5bb5d11209324bd00239448528bd5fb6d.diff";
      sha256 = "sha256-6GyCvZbuquVS++xR68e+jb4IiFPlIbbJb/kmc9uTers=";
    }
  ];

  overlay = (self: super: super.lib.optionalAttrs (super.stdenv.targetPlatform.isAndroid) {
    log = super.runCommandNoCC "log-fake" { } ''
      mkdir -p $out
      touch $out/dummy-log
    '';

    mkDerivation = drv: super.mkDerivation (drv // {
      enableHardening = [ "pie" ];
    });
  });

  # Actually patch our nixpkgs
  patchedNixpkgs = pkgs-pre.applyPatches {
    name = "patched-nixpkgs";
    src = ./submodules/nixpkgs;
    patches = map pkgs-pre.fetchpatch remotePatches;
  };

  # Our final packages with the patched commits
  pkgs = import patchedNixpkgs (haskell-nix.nixpkgsArgs // { config.overlays = [ overlay ]; config.android_sdk.accept_license = true; config.allowUnfree = true; } // nixpkgsArgs);

  mklibcabal = pkgsrc: pkgs.runCommandNoCC "modify-src" { } ''
    set -eux
    mkdir -p $out
    cp -r ${pkgsrc}/* $out
    ls -la $out
    sed -i 's%^executable *\(.*\)$%executable lib\1.so%i' $out/*.cabal
  '';
in
(pkgs.haskell-nix.project' ({
  inherit name compiler-nix-name;
  crossPlatforms = p: [ p.aarch64-android ];
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    inherit name;
    src = mklibcabal src;
  };
  modules = [
    { packages."${name}".components = extraSrcFiles; }
    ({ config, lib, ... }: {
      config.preBuild = ''
        echo "!!! Save Splices $out/lib/haskell.nix/$pname"
        export EXTERNAL_SPLICES_SAVE="$out/lib/haskell.nix/$pname"
      '';
    })
  ] ++ overrides;
})).extend (final: prev: rec {

  # This constructs a "fake" hackage to pull different packages from
  # this is used in case that something on proper hackage doesn't have 
  # the version bounds for packages that we need to properly solve
  # the current project

  # Outputs:
  #  buildCommands - commands to build the generatedHackage jsons
  #  generatedHackage - generated hackage setup
  #  package-overlays - overlays to setup src properly after the solver has succeeded
  #  extra-hackage-tarballs - generated tarballs to be passed to the cabal solver
  #  extra-hackages - alias to (import generatedHackage) - use this in the project'
  hackage-driver = import ./modules/hackage-driver.nix { pkgs = pkgs-pre; modules = hackageOverlays; };
 
  android = (import ./modules/android/default.nix {
    inherit (pkgs) pkgs buildPackages;
    acceptAndroidSdkLicenses = true;
    # Pass the crossPkgs android-prebuilt package set
    pkg-set = crossSystems.aarch64-android-prebuilt.pkg-set;
  });

  android-x86 = (import ./modules/android/default.nix {
    inherit (pkgs) pkgs buildPackages;
    acceptAndroidSdkLicenses = true;
    pkg-set = crossSystems.x86_64-linux-android-prebuilt.pkg-set;
  });

  app = android.buildApp {
    package = p: p.reflex-todomvc.components.reflex-todomvc;
    executableName = "reflex-todomvc";
    applicationId = "org.reflexfrp.todomvc";
    displayName = "Reflex TodoMVC";
  };

  x86-app = android-x86.buildApp {
    package = p: p.reflex-todomvc.components.reflex-todomvc;
    executableName = "reflex-todomvc";
    applicationId = "org.reflexfrp.todomvc";
    displayName = "Reflex TodoMVC";
  };

  # Usage of cross-driver sets up all of the various splices cruft to
  # make an easy way to setup cross-compiling with splices
  crossSystems = builtins.mapAttrs
    (a: v: import ./modules/cross-driver.nix {
      haskell-nix = ../haskell.nix;
      plan-pkgs = import (final.plan-nix);
      inherit name;
      #compiler-nix-name;
      compiler-nix-name = if a == "ghcjs" then "ghc8107" else compiler-nix-name;
      src = mklibcabal src;
      inherit (hackage-driver) extra-hackage-tarballs extra-hackages;
      inherit (final) pkg-set;
      crossPkgs = v;
      splice-driver = import ./modules/splice-driver.nix { dontSplice = [ "fgl" "Cabal" "android-activity" ] ++ dontSplice; };
      hardening-driver = import ./modules/hardening-driver.nix { dontHarden = [ "happy" "binary" "${name}" ] ++ dontHarden; hardeningOpts = hardeningOpts; };
      overrides = [
        # Move this later, not hacky but should be in android configs specifically
        ({ config, lib, pkgs, ... }: {
          packages.${name} = {
            components.exes = lib.optionalAttrs (pkgs.stdenv.targetPlatform.isAndroid) {
              "lib${name}.so" = {
                #hardeningDisable = [ "pie" ];
                ghcOptions = [
                  "-shared"
                  "-fPIC"
                  "-threaded"
                  "-no-hs-main"
                  "-lHSrts_thr"
                  "-lffi"
                  "-lm"
                  "-llog"
                ];
                configureFlags = [
                  "--extra-lib-dirs=${pkgs.androidndkPkgs_23b.libraries}"
                  "--ld-options=-shared"
                  "--ld-options=-no-pie"
                  "--ld-options=-Wl,--gc-sections,--version-script=${./exts/android/haskellActivity.version},-u,Java_systems_obsidian_HaskellActivity_haskellStartMain,-u,hs_main"
                ];
              };
            };
          };
        })
      ] ++ overrides ++ final.hackage-driver.package-overlays;
    })
    pkgs.pkgsCross;
})
