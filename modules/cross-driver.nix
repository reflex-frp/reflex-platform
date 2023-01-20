# NOTE: Cross Driver
# This sets up a <system> that has splice loading enabled
# We currently use a "splice-driver" to do all of the dirty work regarding setting-up "preBuild"
# to load splices

{ name, 
  src, 
  flags ? [ ], 
  crossPkgs, 
  splice-driver, 
  hardening-driver, 
  compiler-nix-name, 
  overrides ? [ ], 
  pkg-set, 
  spliced-packages ? pkg-set, 
  ... 
}@args: let
  filterStdenv = attrs: builtins.listToAttrs (builtins.concatMap (a: if crossPkgs.lib.hasPrefix "is" a then [{ name = if a == "isiOS" then "isIos" else a; value = attrs.${a}; }] else []) (builtins.attrNames attrs));
in crossPkgs.haskell-nix.project' {
  inherit name;
  src = crossPkgs.haskell-nix.haskellLib.cleanGit {
    inherit name src;
  };
  inherit (args) extra-hackages extra-hackage-tarballs;
  index-state = args.index-state or null;
  inherit compiler-nix-name;
  modules = [
    ({ config, lib, ... }: { packages.Cabal.patches = lib.mkForce [ ]; })
    # NOTE: Set compiler properly
    ({ config, lib, ... }: {
      config.compiler.nix-name = lib.mkForce (compiler-nix-name);
    })
    { cabal.system = filterStdenv crossPkgs.stdenv.hostPlatform; }
    { packages.${name}.components.library.ghcOptions = flags; }
  ] ++ overrides ++ (splice-driver {
    attrs = pkg-set.config.packages;
    string = (aname: cname: subname:
      if cname == "library" then ''
        echo "!!! Loading Splices ${spliced-packages.config.hsPkgs.${aname}.components.library}/lib/haskell.nix/$pname"
        export EXTERNAL_SPLICES_LOAD="${spliced-packages.config.hsPkgs.${aname}.components.library}/lib/haskell.nix/$pname"
      '' else ''
        echo "!!! Loading Splices ${spliced-packages.config.hsPkgs.${aname}.components.${cname}.${subname}}/lib/haskell.nix/$pname"
        export EXTERNAL_SPLICES_LOAD="${spliced-packages.config.hsPkgs.${aname}.components.${cname}.${subname}}/lib/haskell.nix/$pname"
      '');
    }) ++ (hardening-driver {
      attrs = pkg-set.config.packages;
    });
}
