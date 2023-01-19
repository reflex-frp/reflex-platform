# NOTE: Define the interface
{ name
, compiler-nix-name ? "ghc8107"
, src
, overrides ? [ ]
, extraSrcFiles ? [ ]
, setupCross ? true
, hackageOverlays ? [ ]
, allowUnfree ? false
, android_sdk_accept_license ? false
, nixpkgsArgs ? { }
}:
let
  # TODO:
  # - Remove this let box properly
  # - Allow for pkgs to be overriden

  # Logic to bootstrap packages
  haskell-nix = import ../haskell.nix { };
  pkgs-pre = import haskell-nix.sources.nixpkgs-unstable (haskell-nix.nixpkgsArgs);

  

  # Patch the packages with some commits
  remotePatches = [
    {
      url = "https://github.com/obsidiansystems/nixpkgs/commit/d39ee6b7c45deb224d95f717bd1e6e2144e09dd9.diff";
      sha256 = "sha256-stn4C43O5M0Qk80gj7YK/87qCDflnm/AwYcOXv5fErI=";
    }
    {
      url = "https://github.com/obsidiansystems/nixpkgs/commit/4516c1a5bb5d11209324bd00239448528bd5fb6d.diff";
      sha256 = "sha256-6GyCvZbuquVS++xR68e+jb4IiFPlIbbJb/kmc9uTers=";
    }
    #{
    #  url = "https://github.com/NixOS/nixpkgs/pull/192629.diff";
    #  sha256 = "sha256-0RF4ocBAXn4y+jmfN72uu5FEFmtG7wKO5QmNZCxZ1ww=";
    #}
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

  patchedNixpkgs = pkgs-pre.applyPatches {
    name = "patched-nixpkgs";
    src = ./nixpkgs;
    patches = map pkgs-pre.fetchpatch remotePatches;
  };

  # Our final packages with the patched commits
  pkgs = import patchedNixpkgs (haskell-nix.nixpkgsArgs // { config.overlays = [ overlay ]; config.android_sdk.accept_license = true; config.allowUnfree = true; } // nixpkgsArgs);

  haskellnixCross = builtins.mapAttrs (a: v: import ../haskell.nix { pkgs = v; }) pkgs.pkgsCross;
  androidCabal = pkgsrc: pkgs.runCommandNoCC "modify-src" { } ''
    set -eux
    mkdir -p $out
    cp -r ${pkgsrc}/* $out
    ls -la $out
    sed -i 's%^executable *\(.*\)$%executable lib\1.so\n  cc-options: -shared\n  ld-options: -shared\n  ghc-options: -shared%i' $out/*.cabal
  '';
  x86Cabal = pkgsrc: pkgs.runCommandNoCC "modify-src" { } ''
    set -eux
    mkdir -p $out
    cp -r ${pkgsrc}/* $out
    ls -la $out
    sed -i 's%^executable *\(.*\)$%executable lib\1.so%i' $out/*.cabal
  '';

  suffixSalt = a: builtins.replaceStrings [ "-" "." ] [ "_" "_" ] a;
in
(pkgs.haskell-nix.project' ({
  inherit name compiler-nix-name;
  crossPlatforms = p: [ p.aarch64-android ];
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    inherit name;
    #inherit src;
    src = x86Cabal src;
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
  inherit haskellnixCross;
  # Hackage overlays
  # Needed if a newer version of a package isn't on the current
  # index of hackage
  hackage-driver = import ./modules/hackage-driver.nix { pkgs = pkgs-pre; modules = hackageOverlays; };
 
  android = (import ./modules/android/default.nix {
    inherit (pkgs) pkgs buildPackages;
    acceptAndroidSdkLicenses = true;
    pkg-set = crossSystems.aarch64-android-prebuilt.pkg-set;
  });

  app = android.buildApp {
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
      inherit name compiler-nix-name;
      #inherit src;
      src = androidCabal src;
      inherit (hackage-driver) extra-hackage-tarballs extra-hackages;
      inherit (final) pkg-set;
      #plan-pkgs = import (final.plan-nix);
      crossPkgs = v;
      splice-driver = import ./modules/splice-driver.nix { dontSplice = [ "fgl" "Cabal" "android-activity" ]; };
      overrides = [
        ({ config, lib, pkgs, ... }: {
          packages.${name} = {
            components.exes = lib.optionalAttrs (pkgs.stdenv.targetPlatform.isAndroid) {
              "lib${name}.so" = {
                hardeningDisable = [ "pie" ];
                ghcOptions = [
                  "-shared"
                  "-no-pie"
                  "-no-pie"
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
      flags = [ ];
    })
    pkgs.pkgsCross;
})
