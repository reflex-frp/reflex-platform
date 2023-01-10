{ name, compiler-nix-name ? "ghc8107", src, overrides ? [ ], extraSrcFiles ? [ ] }:
let
  # TODO:
  # - Remove this let box properly
  # - Allow for pkgs to be overriden
  haskell-nix = import ../haskell.nix { };
  pkgs = import haskell-nix.sources.nixpkgs-unstable (haskell-nix.nixpkgsArgs);

  haskellNixCross = import ../haskell.nix { pkgs = pkgs.pkgsCross.raspberryPi; };
  crossPkgs = import haskellNixCross.sources.nixpkgs-unstable haskellNixCross.nixpkgsArgs;

  removeFromList = { toRemove, baseList }: builtins.attrNames (removeAttrs (builtins.listToAttrs (builtins.concatMap (a: [{ name = a; value = a; }]) baseList)) toRemove);

in
(pkgs.haskell-nix.project' ({
  inherit name compiler-nix-name;
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    inherit name src;
  };
  modules = [
    { packages."${name}".components = extraSrcFiles; }
    ({ config, lib, ... }: {         
        config.preBuild = ''
          echo "!!! Save Splices $out/lib/haskell.nix/$pname"
          export EXTERNAL_SPLICES_SAVE="$out/lib/haskell.nix/$pname"
        '';
      })
    ({ config, lib, ... }: { packages.Cabal.patches = lib.mkForce [ ]; })
  ] ++ overrides;
})).extend (final: prev: rec {
  inherit compiler-nix-name;

  thing = { attrs, string }: builtins.concatMap (aname: let
    componentnames =  pkgs.lib.remove "setup" (pkgs.lib.remove "library" (builtins.attrNames attrs.${aname}.components));
    split = builtins.concatMap (cname: builtins.concatMap (subname: 
        if cname == "library" then 
          [{ packages.${aname}.components.${cname}.preBuild = string aname cname cname; }] 
        else  
          [{ packages.${aname}.components.${cname}.${subname}.preBuild = string aname cname subname; }]
      ) 
      (builtins.attrNames attrs.${aname}.components.${cname})) componentnames;
  in
  [
    {
      packages.${aname}.components = {
        library.preBuild = string aname "library" "library";
      };
    }
  ] ++ split
  ) (removeFromList {
      toRemove = [ "fgl" ];
      baseList = (builtins.attrNames attrs);
    });

  crossCompiled = crossPkgs.pkgsCross.aarch64-multiplatform.haskell-nix.mkCabalProjectPkgSet {
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
      })
    ] ++ overrides ++ (thing {
      attrs = final.pkg-set.config.packages;
      string = (aname: cname: subname: if cname == "library" then ''
        echo "!!! Loading Splices ${saves.config.hsPkgs.${aname}.components.library}/lib/haskell.nix/$pname"
        export EXTERNAL_SPLICES_LOAD="${saves.config.hsPkgs.${aname}.components.library}/lib/haskell.nix/$pname"
      '' else ''
        echo "!!! Loading Splices ${saves.config.hsPkgs.${aname}.components.${cname}.${subname}}/lib/haskell.nix/$pname"
        export EXTERNAL_SPLICES_LOAD="${saves.config.hsPkgs.${aname}.components.${cname}.${subname}}/lib/haskell.nix/$pname"
      '');
    });
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
      ({ config, lib, ... }: {
        /*config.packages = setupSplices {
          attrs = saves.config.packages;
          string-func = (name: component: componentname: if componentname == "library" then ''
            echo "!!! Loading Splices ${final.saves.config.hsPkgs.${name}.components.${componentname}}/lib/haskell.nix/$pname"
            export EXTERNAL_SPLICES_LOAD="${final.saves.config.hsPkgs.${name}.components.${componentname}}/lib/haskell.nix/$pname"
          '' else ''
            echo "!!! Loading Splices ${final.saves.config.hsPkgs.${name}.components.${component}.${componentname}}/lib/haskell.nix/$pname"
            export EXTERNAL_SPLICES_LOAD="${final.saves.config.hsPkgs.${name}.components.${component}.${componentname}}/lib/haskell.nix/$pname"
          '');
          };
        */
        config.preBuild = (thing: thing2: null);
      })
    ] ++ overrides;
  };
})
