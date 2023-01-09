{ name, compiler-nix-name ? "ghc8107", src, overrides ? [ ], extraSrcFiles ? [ ] }:
let
  # TODO:
  # - Remove this let box properly
  # - Allow for pkgs to be overriden
  haskell-nix = import ../haskell.nix { };
  pkgs = import haskell-nix.sources.nixpkgs-unstable haskell-nix.nixpkgsArgs;

  haskellNixCross = import ../haskell.nix { pkgs = pkgs.pkgsCross.raspberryPi; };
  crossPkgs = import haskellNixCross.sources.nixpkgs-unstable haskellNixCross.nixpkgsArgs;

    collectComponents = attrs: string-func: let
      component-func = component: name: builtins.listToAttrs (builtins.concatMap (a: [{ name = a; value = attrs.${name}.components.${component}.${a} // { preBuild = string-func name component a; }; }]) (builtins.attrNames attrs.${name}.components.${component}));
    in builtins.listToAttrs (builtins.concatMap (a: [{
        name = a; 
        # Happy doesn't like us touching it's preBuild's
        value = if a == "happy" || a == "fgl" || a == null then attrs.${a} else {
          components = {
            exes = component-func "exes" a;
            sublibs = component-func "sublibs" a;
            benchmarks = component-func "benchmarks" a;
            tests = component-func "tests" a;
            library = attrs.${a}.components.library // { preBuild = string-func a "library" "library"; };
          } // builtins.removeAttrs attrs.${a}.components [ "exes" "sublibs" "benchmarks" "tests" "library" ];
        } // builtins.removeAttrs attrs.${a} [ "components" ];
      }]) (builtins.attrNames attrs));

    setupSplices = { attrs ? {}, string-func }: collectComponents attrs string-func;
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

  crossProject = crossPkgs.haskell-nix.mkCabalProjectPkgSet {
    plan-pkgs = import ("${prev.plan-nix}/default.nix");
    pkg-def-extras = [ ];
    modules = [
      ({ config, lib, ... }: { 
        packages.Cabal.patches = lib.mkForce [ ]; 
        _module.args.pkgs = lib.mkForce crossPkgs.pkgsCross.raspberryPi;
      })
      ({ config, lib, ... }: { config.compiler.nix-name = lib.mkForce compiler-nix-name; })
      /*({ config, lib, ... }: {
        config.packages = setupSplices {
          attrs = saves.config.packages;
          string-func = (name: component: componentname: if componentname == "library" then ''
            echo "!!! Loading Splices ${saves.config.hsPkgs.${name}.components.${componentname}}/lib/haskell.nix/$pname"
            export EXTERNAL_SPLICES_LOAD="${saves.config.hsPkgs.${name}.components.${componentname}}/lib/haskell.nix/$pname"
          '' else ''
            echo "!!! Loading Splices ${saves.config.hsPkgs.${name}.components.${component}.${componentname}}/lib/haskell.nix/$pname"
            export EXTERNAL_SPLICES_LOAD="${saves.config.hsPkgs.${name}.components.${component}.${componentname}}/lib/haskell.nix/$pname"
          '');
        };
        })
      */
    ];
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
        config.packages = setupSplices {
          attrs = saves.config.packages;
          string-func = (name: component: componentname: if componentname == "library" then ''
            echo "!!! Loading Splices ${saves.config.hsPkgs.${name}.components.${componentname}}/lib/haskell.nix/$pname"
            export EXTERNAL_SPLICES_LOAD="${saves.config.hsPkgs.${name}.components.${componentname}}/lib/haskell.nix/$pname"
          '' else ''
            echo "!!! Loading Splices ${saves.config.hsPkgs.${name}.components.${component}.${componentname}}/lib/haskell.nix/$pname"
            export EXTERNAL_SPLICES_LOAD="${saves.config.hsPkgs.${name}.components.${component}.${componentname}}/lib/haskell.nix/$pname"
          '');
        };
      })
    ] ++ overrides;
  };
})
