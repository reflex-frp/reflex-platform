# NOTE: Define the interface
{ name, 
  compiler-nix-name ? "ghc8107",
  src, 
  overrides ? [ ], 
  extraSrcFiles ? [ ]
}:
let
  # TODO:
  # - Remove this let box properly
  # - Allow for pkgs to be overriden
  haskell-nix = import ../haskell.nix { };
  pkgs-pre = import haskell-nix.sources.nixpkgs-unstable (haskell-nix.nixpkgsArgs);

  remotePatches = [
   {
     url = "https://github.com/obsidiansystems/nixpkgs/commit/d39ee6b7c45deb224d95f717bd1e6e2144e09dd9.diff";
     sha256 = "sha256-stn4C43O5M0Qk80gj7YK/87qCDflnm/AwYcOXv5fErI=";
   }
  ];

  patchedNixpkgs = pkgs-pre.applyPatches {
    name = "patched-nixpkgs";
    src = pkgs-pre.path;
    patches = map pkgs-pre.fetchpatch remotePatches;
  };
 
  pkgs = import patchedNixpkgs (haskell-nix.nixpkgsArgs);
 

  androidCabal = pkgsrc: pkgs.runCommandNoCC "modify-src" {  } ''
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
  # Usage of cross-driver sets up all of the various splices cruft to
  # make an easy way to setup cross-compiling with splices

  android = import ./modules/android/default.nix {
    inherit (pkgs) pkgs buildPackages;
    acceptAndroidSdkLicenses = true;
    pkg-set = crossSystems.aarch64-android.pkg-set;
  };

  crossSystems = builtins.mapAttrs (a: v: import ./modules/cross-driver.nix {
    plan-pkgs = import ("${prev.plan-nix}/default.nix");
    projectName = name;
    #inherit (pkgs) pkgsCross;
    crossPkgs = v;
    inherit compiler-nix-name;
    inherit (final) pkg-set;
    #spliced-packages = final.pkg-set;
    splice-driver = import ./modules/splice-driver.nix { dontSplice = [ "fgl" "Cabal" ]; };
    overrides = (if a == "aarch64-android" then
    [{ 
      packages.${name}.components.library = { 
        ghcOptions = [ 
          #"-shared"
          "-fPIC"
          "-threaded"
          "-no-hs-main"
          "-lHSrts_thr"
          "-lffi"
          "-lm"
          "-llog"
        ];
        configureFlags = [
          #"--cc-options=-shared"
          #"--cc-options=-fPIC"
          #"--ld-options=-shared"
          #"--ld-options=-Wl,--gc-sections,--version-script=${./exts/android/haskellActivity.version},-u,Java_systems_obsidian_HaskellActivity_haskellStartMain,-u,hs_main"
        ];
    };
    }]
    else []) ++ overrides;
    flags = [ ];
  }) pkgs.pkgsCross;
})
