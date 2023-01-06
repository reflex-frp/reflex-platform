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

    filter-things = attrs: pkgs.lib.mapAttrsRecursiveCond (a: a ? components && a.components ? library && a ? type && builtins.trace (a.components) a.type == "derivation") (v: c: "null") attrs;

    # NOTE(Dylan Green): This is how we setup loading splices
    # Currently this is ugly and somewhat heurstical
    # We check for certain properties in an attrset and re-run these functions based on that
    # It's ugly and we probably shouldn't be doing this as it has some downsides
    # though this is currently the easiest way to provide a PoC
    # it also "collects" all errors within the attrset, which is an issue, since one "misconfigured" package breaks
    # the whole function since we're traversing attrsets and the module system immediately will error
    # before the output attrset is provided
    # Some of these packages aren't meant to be overriden which also causes issues
    # mostly BNFC at the moment

    filterPackagesetToList = attrs: basepkgs: (builtins.concatMap
      (a:
        let
          v = attrs.${a};
          recurse = if builtins.trace ("${a} recursing") builtins.isAttrs attrs.${a} then filterPackagesetToList attrs.${a} basepkgs
          #filterPackagesetToList attrs.${a} 
          else (builtins.trace ("Exiting Loop") v);
        in
        if (!attrs.${a} ? components) && attrs.${a} ? "preBuild" then builtins.trace ("Modifying preBuild of ${a}...") [{
          name = toString a;
          value = ({ preBuild = basepkgs "echo $pname"; } // builtins.removeAttrs attrs.${a} [ "preBuild" ]);
        }] else let 
            checkList = if builtins.isList recurse && !(builtins.all builtins.isString recurse) then (builtins.listToAttrs) recurse else v;
        in [{ name = if !builtins.isString a then "null" else toString a; value = checkList; }])
        (builtins.attrNames attrs));
     
        fix = { brokenpkgs ? [], healedattrs, attrs }: builtins.removeAttrs attrs brokenpkgs // builtins.listToAttrs (map (c: { name = "${c}"; value = healedattrs.${c}; }) brokenpkgs);

    filter-packageset = attrs: basepkgs: builtins.listToAttrs (filterPackagesetToList attrs basepkgs);

 /* # NOTE: Function to make defining loading/saving steps less tedious  
  load-save-func = { components ? [ ], load-string, pkgset, elseState }: builtins.listToAttrs (builtins.concatLists 
  (map (component: [ {
    name = "${component}";
    value = builtins.listToAttrs (builtins.concatMap (v: [{ name = "modified"; value = pkgs.lib.mapAttrsRecursiveCond v
    (x: c: let
      splitModule = pkgs.lib.splitString "." "${component}";
      basemodule = builtins.head (splitModule);
      tailmodule = builtins.toString (builtins.tail (splitModule));
    in
    if c ? "${basemodule}" then 
      if c."${basemodule}" ? "${tailmodule}" then
        (builtins.removeAttrs c."${basemodule}" [ "${tailmodule}" ]) // {
          "${basemodule}" = {
            "${tailmodule}" = {
              preBuild = load-string x c;
            } // (if c."${basemodule}"."${tailmodule}" == null then { } else (builtins.removeAttrs c."${basemodule}"."${tailmodule}" [ "preBuild" ]));
          } // (builtins.removeAttrs c."${basemodule}" [ "${tailmodule}" ]);
        } else (builtins.removeAttrs c [ "${basemodule}" ]) // {
          "${basemodule}" = {
            preBuild = load-string x c;
          } // (if c."${basemodule}" == null then { } else (builtins.removeAttrs c."${basemodule}" [ "preBuild" ]));
        } else c)
        pkgset; }])
      elseState);  
    }]) 
    components));
  */

  load-save-func-2 = { load-string, pkgset }: pkgs.lib.mapAttrs (a: v: v // { allComponent = ({ preBuild = pkgs.lib.mkForce load-string a v; } // builtins.removeAttrs v.allComponent [ "preBuild" "profilingDetail" ]); }) pkgset;
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

  test2 = filter-packageset final.pkg-set.config.packages (builtins.attrNames final.pkg-set.config.packages);
  test4 = fix {
    brokenpkgs = [ 
      "BNFC" 
      "GLFW-b"
    ];
    attrs = final.pkg-set.config.packages;
  };

  test = load-save-func-2 rec {
          #components = [ "library" "exes.reflex-todomvc" ];
          load-string = (x: c: ''
            echo "!!! Save Splices $out/lib/haskell.nix/${builtins.head x}"
            export EXTERNAL_SPLICES_SAVE="$out/lib/haskell.nix/${builtins.head x}"
          '');
          pkgset = final.pkg-set.config.packages;
          #elseState = map (component: (as: if !(as ? components) then false else if as.components ? "${component}" then true else false)) components;
        };
  
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
        config.preBuild = ''
          echo "!!! Save Splices $out/lib/haskell.nix/$pname"
          export EXTERNAL_SPLICES_SAVE="$out/lib/haskell.nix/$pname"
        '';
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
      ({ config, lib, ... }: { packages.BNFC = {}; })
      /*({ config, lib, ... }: { 
        config.preBuild = ''
          export NIX_PATH="nixpkgs=${pkgs.path}"
          export EXTERNAL_SPLICES_SAVE=$(${pkgs.nix}/bin/nix-build --no-build-output ${src}/default.nix -A saves.config.hsPkgs.$name.components.library)/lib/haskell.nix/$pname/
        '';
      })*/
      ({ config, lib, ... }: {
        #config.packages = filter-packageset saves.config.packages;
        #config.packages = (builtins.mapAttrs (v: c: { allComponent = { preBuild = lib.mkForce "exit && null"; } // builtins.removeAttrs c.allComponent [ "preBuild" ]; } // builtins.removeAttrs c [ "allComponent" ]) final.pkg-set.config.packages);
        config.packages = (fix {
          brokenpkgs = [ 
            "lens"
            "BNFC" 
            "GLFW-b"
            "Sit"
            "Win32"
            "X11"
            "bindings-GLFW"
            "ghc"
            "ghci"
            "ghcide"
            "cabal-install"
            "closed"
            "iserv"
            "libiserv"
            "cpphs"
            "cryptonite-openssl"
            "discount"
            "ghc-boot"
            "ghc-boot-th"
            "ghc-heap"
            "ghc-lib-parser"
            "hnix"
            "hpc"
            "iserv-proxy"
            "language-c"
            "llvm-hs"
            "mintty"
            "odbc"
            "pcap"
            "polyparse"
            "remote-iserv"
            "rts"
            "terminfo"
            "x509-system"
          ];
          attrs = filter-packageset final.pkg-set.config.packages (builtins.attrNames final.pkg-set.config.packages);
          healedattrs = final.pkg-set.config.packages;
        });
        /*config.packages = (load-save-func rec {
          components = [ 
            "library"
          ];
          # NOTE: We have to implement logic here to properly check if the directory exists
          # or the compiler gets incredibly upset and breaks
          load-string = (x: c:
            let
              dir = "${saves.config.hsPkgs.${builtins.head x}.components.library}/lib/haskell.nix/$pname";
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
          elseState = (map (v: (as: if !(as ? components) then false else if as.components ? "${v}" then true else false)) components);
          }).library.modified;
        */
      })
    ] ++ overrides;
  };
})
