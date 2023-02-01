final: prev: let
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
