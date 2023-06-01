{ pkgs, inputMap, cabalProject }@args: let
  unpackPackage = path: pkgs.runCommandNoCC "unpack-package" { } ''
    mkdir -p $out
    if [[ $path == *.tar.gz ]]; then
       tar -xvf ${path} -C $out
       cp -r $out/*/* $out
    else
      cp -r ${path}/* $out
    fi
    echo -e "packages: .\n" > $out/cabal.project
  '';
in
  { package, pkg-set, compiler-nix-name, constraints ? [], allowNewer ? [], allowOlder ? [], ... }: let

  constraintsToString = [ ("constraints: " + builtins.concatStringsSep "," constraints) ];

  allowN = [ ("allow-newer: " + builtins.concatStringsSep "," allowNewer) ];
  allowO = [ ("allow-older: " + builtins.concatStringsSep "," allowOlder) ];

  src-driver = import ./src-driver.nix {
    inherit pkgs;
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = pkg-set."${package}".name;
      src = unpackPackage (pkg-set."${package}".src);
    };
    hackage = [];
    extraCabalProject = args.cabalProject or []
      ++ pkgs.lib.optionals (constraints != []) constraintsToString
      ++ pkgs.lib.optionals (allowNewer != []) allowN
      ++ pkgs.lib.optionals (allowOlder != []) allowO;
  };

  splitDrvName = builtins.parseDrvName (pkg-set."${package}".name);

  proj = pkgs.haskell-nix.project' {
    name = splitDrvName.name;
    src = src-driver;
    inherit compiler-nix-name;
    inherit inputMap;
    modules = [
      ({ config, pkgs, lib, ... }: {
        config.doHaddock = false;
      })
    ];
  };
in proj.extend (self: super: {
  src = unpackPackage (pkg-set."${package}".src);
  shell = self.shellFor {
    packages = ps: [ ps.${splitDrvName.name} ];
    tools = {
      cabal-install = "3.4.0.0";
    };
  };
})
