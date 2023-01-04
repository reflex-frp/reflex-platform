{ name, compiler-nix-name ? "ghc8107", src, overrides ? [ ], extraSrcFiles ? [ ] }:
let
  # TODO:
  # - Remove this let box properly
  # - Allow for pkgs to be overriden
  haskell-nix = import
    (builtins.fetchGit {
      url = "https://github.com/obsidiansystems/haskell.nix.git";
      ref = "dylang/splices";
      rev = "80ebfa60685c4f0d3fd0ec7c5f77ffa9b08bd04e";
    })
    { };

  pkgs = import haskell-nix.sources.nixpkgs-unstable haskell-nix.nixpkgsArgs;

  # NOTE: Function to make defining loading/saving steps less tedious  
  load-save-func = { component, load-string, pkgset, elseState }: pkgs.lib.mapAttrsRecursiveCond elseState
    (x: c:
      if c ? "${component}" then (builtins.removeAttrs c [ "${component}" ]) // {
        "${component}" = {
          preBuild = load-string x c;
        } // (if c."${component}" == null then { } else (builtins.removeAttrs c."${component}" [ "preBuild" ]));
      } else c)
    pkgset;
in
(pkgs.haskell-nix.project' ({
  inherit name compiler-nix-name;
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    inherit name src;
  };
  modules = [
    { packages."${name}".components = extraSrcFiles; }
    ({ config, lib, ... }: { packages.Cabal.patches = lib.mkForce [ ]; })
  ] ++ overrides;
})).extend (final: prev: rec {
  inherit compiler-nix-name;
  
  # NOTE: Modify the base package set to save splices
  saves = pkgs.haskell-nix.mkCabalProjectPkgSet {
    plan-pkgs = import ("${prev.plan-nix}/default.nix");
    pkg-def-extras = [ ];
    inherit compiler-nix-name;
    modules = [
      ({ config, lib, ... }: { packages.Cabal.patches = lib.mkForce [ ]; })
      # NOTE: Set compiler properly
      ({ config, lib, ... }: {
        config.compiler.nix-name = lib.mkForce (compiler-nix-name);
      })
      ({ config, lib, ... }: {
        # NOTE: Haskell.nix components don't have the best way to implement this,
        # so we do this in preBuild
        config.packages = load-save-func rec {
          component = "library";
          load-string = (x: c: ''
            echo "!!! Save Splices $out/lib/haskell.nix/${builtins.head x}"
            export EXTERNAL_SPLICES_SAVE="$out/lib/haskell.nix/${builtins.head x}"
          '');
          pkgset = final.pkg-set.config.packages;
          elseState = (as: if !(as ? components) then false else if as.components ? "${component}" then true else false);
        };
      })
    ] ++ overrides;
  };

  # NOTE: Modify the base package set to load from the splices packageset
  loads = pkgs.haskell-nix.mkCabalProjectPkgSet {
    plan-pkgs = import ("${final.plan-nix}/default.nix");
    pkg-def-extras = [ ];
    inherit compiler-nix-name;
    modules = [
      # NOTE: Set compiler properly
      ({ config, lib, ... }: { config.compiler.nix-name = lib.mkForce (compiler-nix-name); })
      ({ config, lib, ... }: {
        config.packages = load-save-func rec {
          component = "library";
          # NOTE: We have to implement logic here to properly check if the directory exists
          # or the compiler gets incredibly upset and breaks
          load-string = (x: c:
            let
              dir = "${saves.config.hsPkgs.${builtins.head x}.components.library}/lib/haskell.nix/${builtins.head x}";
            in
            ''
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
          pkgset = final.pkg-set.config.packages;
          elseState = (as: if !(as ? components) then false else if as.components ? "${component}" then true else false);
        };
      })
    ] ++ overrides;
  };
})
