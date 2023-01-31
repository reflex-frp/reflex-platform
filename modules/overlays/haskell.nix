final: prev: let
  callCabal2Nix = compiler-nix-name: name: src: final.buildPackages.stdenv.mkDerivation {
    name = "${name}-package.nix";
    inherit src;
    nativeBuildInputs = [
      # It is not safe to check the nix-tools materialization here
      # as we would need to run this code to do so leading to
      # infinite recursion (so using nix-tools-unchecked).
      final.buildPackages.haskell-nix.nix-tools-unchecked.${compiler-nix-name}
    ];
    phases = [ "unpackPhase" "buildPhase" ];

    LOCALE_ARCHIVE = final.lib.optionalString (final.stdenv.hostPlatform.libc == "glibc") "${final.glibcLocales}/lib/locale/locale-archive";
    LANG = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";

    buildPhase = ''
      sed -i 's/^cabal-version: *2\.1/cabal-version: 3.0/' *.cabal
      cabal-to-nix *.cabal > $out
    '';
  };

  # Combines multiple derivations into one to make them
  # easier to materialize.
  # Using `cp -Lr` here follows the symlinks and prevents
  # `access to path is forbidden in restricted mode`
  # errors on hydra when the materialized files are not present.
  combineFiles = name: ext: files:
    let links = final.linkFarm name
      (final.lib.mapAttrsToList (name: path: {
        name = name + ext;
        inherit path;
      }) files);
    in final.buildPackages.runCommand "${name}${ext}" {} ''
      cp -Lr ${links} $out
      chmod -R +w $out
    '';

  # Combine the all the boot package nix files for a given ghc
  # into a single derivation and materialize it.
  combineAndMaterialize = unchecked: ghcName: bootPackages:
  (final.haskell-nix.materialize ({
          materialized = ../../materialized/ghc-boot-packages-nix + "/${ghcName +
              # The 3434.patch we apply to fix linking on arm systems changes ghc-prim.cabal
              # so it needs its own materialization.
              final.lib.optionalString final.targetPlatform.isAarch64 "-aarch64"
            }";
        } // final.lib.optionalAttrs unchecked {
          checkMaterialization = false;
        }) (combineFiles "${ghcName}-boot-packages-nix" ".nix" (builtins.mapAttrs
          (_: srcAndNix: srcAndNix.nix) bootPackages)));

  # Import the nix and src.
  importSrcAndNix = srcAndNix:
      args: (import srcAndNix.nix args) // { inherit (srcAndNix) src; };

  # The packages in GHC source and the locations of them
  ghc-extra-pkgs = ghcVersion: {
      base         = "libraries/base";
      bytestring   = "libraries/bytestring";
      ghci         = "libraries/ghci";
      ghc-heap     = "libraries/ghc-heap";
      ghc-prim     = "libraries/ghc-prim";
      hpc          = "libraries/hpc";
      integer-gmp  = "libraries/integer-gmp";
      libiserv     = "libraries/libiserv";
      template-haskell = "libraries/template-haskell";
      iserv        = "utils/iserv";
      iserv-proxy  = "utils/iserv-proxy";
      Win32        = "libraries/Win32";
    } // final.lib.optionalAttrs (!final.stdenv.hostPlatform.isGhcjs) {
      ghc          = "compiler";
      ghc-boot     = "libraries/ghc-boot";
    } // final.lib.optionalAttrs (!final.stdenv.hostPlatform.isGhcjs || builtins.compareVersions ghcVersion "8.10.5" >= 0) {
      # Not sure why, but this is missing from older ghcjs versions
      remote-iserv = "utils/remote-iserv";
    } // final.lib.optionalAttrs (builtins.compareVersions ghcVersion "9.0.1" >= 0) {
      ghc-bignum   = "libraries/ghc-bignum";
    } // final.lib.optionalAttrs (builtins.compareVersions ghcVersion "9.2.1" >= 0) {
      deepseq      = "libraries/deepseq";
      pretty       = "libraries/pretty";
    };

  # The nix produced by `cabalProject` differs slightly depending on
  # what the platforms are.  There are currently 3 possible outputs.
  ghc-extra-projects-type =
    if final.stdenv.hostPlatform.isWindows
      then "windows"
      else if final.stdenv.hostPlatform.isGhcjs
        then "ghcjs"
        else if final.haskell-nix.haskellLib.isCrossHost
          then "cross"
          else "default";
in rec {
  obsidian-materialization  = builtins.mapAttrs
    (combineAndMaterialize true)
    final.ghc-boot-packages-src-and-nix;

  obsidian-materialization-unchecked  = builtins.mapAttrs
    (combineAndMaterialize false)
      final.ghc-boot-packages-src-and-nix;

  ghc-boot-packages = builtins.mapAttrs
    (ghcName: value: builtins.mapAttrs
      (pkgName: srcAndNix: importSrcAndNix {
        inherit (srcAndNix) src;
        nix = final.ghc-boot-packages-nix.${ghcName} + "/${pkgName}.nix";
      }) value)
      final.ghc-boot-packages-src-and-nix;

  ghc-boot-packages-nix = prev.ghc-boot-packages-nix // {
    ghc8107Splices = obsidian-materialization.ghc8107Splices;
  };

  ghc-boot-packages-nix-unchecked = prev.ghc-boot-packages-nix // {
    ghc8107Splices = obsidian-materialization-unchecked.ghc8107Splices;
  };

  ghc-boot-packages-unchecked = builtins.mapAttrs
    (ghcName: value: builtins.mapAttrs
      (pkgName: srcAndNix: importSrcAndNix {
        inherit (srcAndNix) src;
        nix = final.ghc-boot-packages-nix-unchecked.${ghcName} + "/${pkgName}.nix";
      }) value)
      final.ghc-boot-packages-src-and-nix;

  ghc-extra-packages = { };
  haskell-nix = prev.haskell-nix // {
    checkMaterialization = false;
    compiler = prev.haskell-nix.compiler // final.obsidianCompilers.ghc
      // prev.lib.optionalAttrs (final.stdenv.targetPlatform.isGhcjs) final.obsidianCompilers.ghcjs;
  };
}
