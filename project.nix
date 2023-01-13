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
  ];

  patchedNixpkgs = pkgs-pre.applyPatches {
    name = "patched-nixpkgs";
    src = pkgs-pre.path;
    patches = map pkgs-pre.fetchpatch remotePatches;
  };

  # Our final packages with the patched commits
  pkgs = import patchedNixpkgs (haskell-nix.nixpkgsArgs // { config.android_sdk.accept_license = true; config.allowUnfree = true; } // nixpkgsArgs);

  androidCabal = pkgsrc: pkgs.runCommandNoCC "modify-src" { } ''
    set -eux
    mkdir -p $out
    cp -r ${pkgsrc}/* $out
    ls -la $out
    sed -i 's%^executable *\(.*\)$%executable lib\1\n  cc-options: -shared -fPIC\n  ld-options: -shared -Wl,--gc-sections,--version-script=${./exts/android/haskellActivity.version},-u,Java_systems_obsidian_HaskellActivity_haskellStartMain,-u,hs_main\n  ghc-options: -shared -fPIC -threaded -no-hs-main -lHSrts_thr -lffi -lm -llog%i' $out/*.cabal
  '';
in
(pkgs.haskell-nix.project' ({
  inherit name compiler-nix-name;
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    inherit name src;
    #src = androidCabal src;
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

  # Hackage overlays
  # Needed if a newer version of a package isn't on the current
  # index of hackage
  hackage-driver = import ./modules/hackage-driver.nix { pkgs = pkgs-pre; modules = hackageOverlays; };
 
  android = import ./modules/android/default.nix {
    inherit (pkgs) pkgs buildPackages;
    acceptAndroidSdkLicenses = true;
    pkg-set = crossSystems.aarch64-android.pkg-set;
  };


  # Usage of cross-driver sets up all of the various splices cruft to
  # make an easy way to setup cross-compiling with splices
  crossSystems = builtins.mapAttrs
    (a: v: import ./modules/cross-driver.nix {
      inherit name src compiler-nix-name;
      inherit (hackage-driver) extra-hackage-tarballs extra-hackages;
      inherit (final) pkg-set;
      crossPkgs = v;
      splice-driver = import ./modules/splice-driver.nix { dontSplice = [ "fgl" "Cabal" ]; };
      overrides = [
        ({ config, lib, pkgs, ... }: {
          packages.${name} = {
            components.library = {
              ghcOptions = lib.optionals (pkgs.stdenv.targetPlatform.isAndroid) [
                #"-shared"
                "-fPIC"
                "-threaded"
                "-no-hs-main"
                "-lHSrts_thr"
                "-lffi"
                "-lm"
                "-llog"
              ];
              configureFlags = lib.optionals (pkgs.stdenv.targetPlatform.isAndroid) [
                #"--cc-options=-shared"
                "--cc-options=-fPIC"
                #"--ld-options=-shared"
                "--ld-options=-Wl,--gc-sections,--version-script=${./exts/android/haskellActivity.version},-u,Java_systems_obsidian_HaskellActivity_haskellStartMain,-u,hs_main"
              ];
            };
            src = androidCabal src;
          };
        })
      ] ++ overrides;
      flags = [ ];
    })
    pkgs.pkgsCross;
})
