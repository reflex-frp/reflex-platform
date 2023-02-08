{ pkgs, obelisk, deps, obsidian }:

{ name # Name of the current project
, compiler-nix-name ? "ghc8107" # What compiler we should be using
, ghcjs-compiler-nix-name ? "ghcjs8107"
, src # Source of the current project
, overrides ? [ ] # Overrides to packages
, extraSrcFiles ? [ ] # ExtraSrcFiles to include in the project builds
, setupCross ? true # Setup cross-compiling
, hackageOverlays ? [ ] # Overlays for hackage, to pass to the cabal solver
, hackage-extra-tarballs ? { }
, extra-hackages ? [ ]
, dontSplice ? [ ] # Packages to not splice
, dontHarden ? [ ] # Packages to not harden
, hardeningOpts ? [ "-fPIC" "-pie" ]
, inputMap ? { }
}@args:

let
  # Driver to generate a fake hackage
  hackage-driver = import ./hackage-driver.nix {
    pkgs = pkgs;
    modules = (hackageOverlays ++ (obelisk pkgs));
  };
  checkHackageOverlays = c: v: if (hackageOverlays) == [ ] then c else v;

  # Base project without any extensions added
  baseProject = pkgs.haskell-nix.project' {
    inherit name compiler-nix-name inputMap;
    extra-hackage-tarballs = (checkHackageOverlays { } hackage-driver.extra-hackage-tarballs) // hackage-extra-tarballs;
    extra-hackages = (checkHackageOverlays [ ] hackage-driver.extra-hackages) ++ extra-hackages;

    src = pkgs.haskell-nix.haskellLib.cleanGit {
      inherit name src;
    };

    modules = [
      { packages."${name}".components = extraSrcFiles; }
      # Setup the saving part of splices unconditionally
      ({ config, lib, ... }: {
        config.preBuild = ''
          echo "!!! Save Splices $out/lib/haskell.nix/$pname"
          export EXTERNAL_SPLICES_SAVE="$out/lib/haskell.nix/$pname"
        '';
      })
    ] ++ overrides;
  };
in
baseProject.extend (final: prev: rec {
  # Self includes everything used in the baseProject
  self = {
    inherit args;
    inherit obsidian pkgs checkHackageOverlays hackage-driver;
    inherit deps baseProject;
  };
  # Null out haskell.nix's default cross setup, since it doesn't work
  # properly
  projectCross = builtins.abort "Haskell.nix projectCross isn't supported!";

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
  inherit hackage-driver;
 # hackage-driver = import ./modules/hackage-driver.nix { pkgs = pkgs; modules = hackageOverlays; };

  android = (import ./android/default.nix {
    inherit (pkgs) pkgs buildPackages;
    acceptAndroidSdkLicenses = true;
    # Pass the crossPkgs android-prebuilt package set
    pkg-set = crossSystems.aarch64-android-prebuilt.pkg-set;
  });

  android-x86 = (import ./android/default.nix {
    inherit (pkgs) pkgs buildPackages;
    acceptAndroidSdkLicenses = true;
    pkg-set = crossSystems.x86_64-linux-android-prebuilt.pkg-set;
  });

  shells = {
    ghc = prev.shell;
    ghcjs = crossSystems.ghcjs.shell;
  };

  # The android app builder currently assumes you just pass the base name of the package
  # to the builder, and we convert it to "lib${name}.so" in there
  app = android.buildApp {
    # Package is currently just filler
    package = p: p.${name}.components.${name};
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

  # Easy way to get to the ghcjs app
  ghcjs-app = crossSystems.ghcjs.pkg-set.config.hsPkgs."${name}".components.exes."${name}";

  # Usage of cross-driver sets up all of the various splices cruft to
  # make an easy way to setup cross-compiling with splices
  crossSystems = builtins.mapAttrs
    (a: v: import ./cross-driver.nix {
      # Project name and source
      inherit name src;

      # Haskell.nix derives is ghcjs off of the compiler-nix-name
      # so ghc8107Splices won't cut it here
      compiler-nix-name = if a == "ghcjs" then ghcjs-compiler-nix-name else compiler-nix-name;

      # Make sure to inherit the proper overrides from the hackage-driver
      # Reference ./modules/hackage-driver.nix for more details
      extra-hackage-tarballs = checkHackageOverlays { } hackage-driver.extra-hackage-tarballs;
      extra-hackages = checkHackageOverlays [ ] hackage-driver.extra-hackages;
      inherit (final) pkg-set;

      # CrossPkgs is the attrset of the current crossSystem in the mapAttrs
      crossPkgs = v;

      # Driver to automatically setup splices
      # Reference ./modules/splice-driver.nix for more details
      splice-driver = import ./splice-driver.nix {
        dontSplice = [ "fgl" "Cabal" "android-activity" ] ++ dontSplice; # Packages to not load splices for
      };

      # Driver to auto-apply hardening options
      # Reference ./modules/hardening-driver.nix for more details
      hardening-driver = import ./hardening-driver.nix {
        dontHarden = [ "happy" "binary" "${name}" ] ++ dontHarden; # Packages to not apply hardening to
        hardeningOpts = hardeningOpts;
      };
      overrides = [
        # Easier override for users to set extra files from the package src to be included in build
        { packages.${name}.components = extraSrcFiles; }

        # Move this later, not hacky but should be in android configs specifically, due to some linker args
        # and how we combine this with gradle
        ({ config, lib, pkgs, ... }: {
          packages.${name} = {
            components.exes = lib.optionalAttrs (pkgs.stdenv.targetPlatform.isAndroid) {
              "${name}" = {
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
                  "--ld-options=-shared"
                  "--ld-options=-no-pie"
                  "--ld-options=-Wl,--gc-sections,--version-script=${../exts/android/haskellActivity.version},-u,Java_systems_obsidian_HaskellActivity_haskellStartMain,-u,hs_main"
                ];
              };
            };
          };
        })
      ] ++ overrides ++ final.hackage-driver.package-overlays;
    })
    pkgs.pkgsCross;
})
