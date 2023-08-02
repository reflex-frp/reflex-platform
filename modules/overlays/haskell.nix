final: prev: rec {
  obsidian-materialization  = builtins.mapAttrs
    (final.combineAndMaterialize true ../../materialized)
      final.ghc-boot-packages-src-and-nix;

  obsidian-materialization-unchecked  = builtins.mapAttrs
    (final.combineAndMaterialize false ../../materialized)
      final.ghc-boot-packages-src-and-nix;

  # Add new compilers boot-packages here
  ghc-boot-packages-nix = prev.ghc-boot-packages-nix // {
    ghc8107Splices = obsidian-materialization.ghc8107Splices;
    ghcjs8107JSString = obsidian-materialization.ghcjs8107JSString;
    ghcjs8107 = obsidian-materialization.ghcjs8107;
  };

  ghc-boot-packages-nix-unchecked = prev.ghc-boot-packages-nix-unchecked // {
    ghc8107Splices = obsidian-materialization-unchecked.ghc8107Splices;
    ghcjs8107JSString = obsidian-materialization-unchecked.ghcjs8107JSString;
    ghcjs8107 = obsidian-materialization-unchecked.ghcjs8107;
  };
  # ---

  haskell-nix = prev.haskell-nix // {
    checkMaterialization = false;
    compiler = prev.haskell-nix.compiler // final.obsidianCompilers.ghc // prev.lib.optionalAttrs (final.stdenv.targetPlatform.isGhcjs or false) final.obsidianCompilers.ghcjs;
    ghcjsProject = import (final._dep.source."haskell.nix" + "/lib/ghcjs-project.nix") {
      pkgs = final;
      materialized-dir = ../../materialized;
    };
  };
}
