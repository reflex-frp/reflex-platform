{ name, compiler-nix-name ? "ghc8107", src, overrides ? [ ], extraSrcFiles ? [] }:
let
  haskell-nix = import (builtins.fetchGit {
    url = "https://github.com/obsidiansystems/haskell.nix.git";
    ref = "dylang/splices";
    rev = "80ebfa60685c4f0d3fd0ec7c5f77ffa9b08bd04e";
  }) { };

  pkgs = import haskell-nix.sources.nixpkgs-unstable haskell-nix.nixpkgsArgs;
  load-save-func = { component, load-string, pkgset, elseState }: pkgs.lib.mapAttrsRecursiveCond elseState
    (x: c: if c ? "${component}" then (builtins.removeAttrs c [ "${component}" ]) // {
      "${component}" = {
        preBuild = load-string x c;
      } // (if c."${component}" == null then {} else (builtins.removeAttrs c."${component}" [ "preBuild" ]));
    } else c) pkgset;

  project-pre' = pkgs.haskell-nix.project' ({
    inherit name compiler-nix-name;
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      inherit name src;
    };
    modules = [
      { packages."${name}".components = extraSrcFiles; }
      ({ config, lib, ... }: { packages.Cabal.patches = lib.mkForce [ ]; })
    ] ++ overrides;
  });
  proj' = pkgs.haskell-nix.project' ({
    inherit name compiler-nix-name;
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      inherit name src;
    };
    modules = [
      ({ config, lib, ... }: { packages.Cabal.patches = lib.mkForce [ ]; })
      ({ config, lib, ... }: {
        config.packages = load-save-func rec {
          component = "library";
          load-string = (x: c: ''
            echo "!!! Save Splices $out/lib/haskell.nix/${builtins.head x}"
            export EXTERNAL_SPLICES_SAVE="$out/lib/haskell.nix/${builtins.head x}"
          '');
          pkgset = project-pre'.pkg-set.config.packages;
          elseState = (as: if !(as ? components) then false else if as.components ? "${component}" then true else false);
        }; 
      })
    ] ++ overrides;
  });
  projSplices = pkgs.haskell-nix.project' ({
    inherit name compiler-nix-name;
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      inherit name src;
    };
    modules = [
      ({ config, lib, ... }: {
        config.packages = load-save-func rec {
          component = "library";
          load-string = (x: c: let
            dir = "${proj'.hsPkgs.${builtins.head x}.components.library}/lib/haskell.nix/${builtins.head x}";
          in ''
            echo "!!! Attempting to load splices ${dir}"
            if [ -d ${dir} ]; then
              if [ "$(ls -A ${dir})" ]; then
                echo "Splices Found!"
                export EXTERNAL_SPLICES_LOAD=${dir}
              else
                echo "No Splices Available"
              fi
            else
              echo "Directory doesn't exist"
            fi
          '');
          pkgset = proj'.pkg-set.config.packages;
          elseState = (as: if !(as ? components) then false else if as.components ? "${component}" then true else false);
        };
      })
    ] ++ overrides;
  });
in projSplices.extend (final: prev: {
  saves = proj';
  base = project-pre';
})
