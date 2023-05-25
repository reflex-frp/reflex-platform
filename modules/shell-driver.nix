{ shells ? (ps: []),
  project,
  crossSystems,
  withHoogle,
  shellTools,
  exactDeps,
  ...
}: shellArgs:
let
  shellDef = {
    targetSystem ? "",
    crossBuilds ? [  ],
    ...
  }@args: let
    crossProjects = map (a: crossSystems."${a}".shellFor {
      packages = ps: shells ps;
      withHoogle = false;
    }) crossBuilds;

    shellSetup = map (config: ''
      alias ${config}-ghc=${crossSystems."${config}".pkgs.stdenv.targetPlatform.config}-ghc
      alias ${config}-cabal=${crossSystems."${config}".pkgs.stdenv.targetPlatform.config}-cabal
    '') crossBuilds;
  in {
    default = project.shellFor {
      packages = ps: shells ps;
      inherit withHoogle exactDeps;
      tools = shellTools;
      inputsFrom = crossProjects;
      shellHook = builtins.concatStringsSep "\n" shellSetup;
    };
    justCross = crossSystems."${targetSystem}".shellFor {
      packages = ps: shells ps;
      inherit withHoogle exactDeps;
      tools = shellTools;
      shellHook = builtins.concatStringSep "\n" shellSetup;
    };
  }."${targetSystem}";

in shellDef shellArgs
