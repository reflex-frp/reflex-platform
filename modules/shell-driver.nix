{ shells ? (ps: []),
  project,
  crossSystems,
  withHoogle,
  shellTools,
  exactDeps,
  unsafeMsg,
  ...
}: shellArgs:

# NOTE: Enable this after were completely deprecated string-shells
#assert project.pkgs.lib.assertMsg (builtins.any (x: (builtins.isAttrs x)) (shells project.pkg-set.config.packages)) "Shell packages can't be strings, please don't wrap in a string!";

let
  _privShell = ps: if builtins.isList shells then shells else shells ps;
  __shells__ = ps: builtins.map (a: if builtins.isAttrs a
                              then a
                              else let
                                pos = builtins.unsafeGetAttrPos "shells" project.__unsafe.bot_args;
                              in (unsafeMsg {
                                attr = "shells";
                                set = project.__unsafe.bot_args;
                                msg = pso: "Please use 'shells = ps: with ps; [ ${a} ]' at line ${toString pos.line} instead of the string ${a}";
                              }) ps."${a}") (_privShell ps);
  shellDef = {
    justCross ? false,
    targetSystem ? "",
    crossBuilds ? [  ],
    ...
  }@args: let
    crossProjects = map (a: crossSystems."${a}".shellFor {
      packages = ps: __shells__ ps;
      withHoogle = false;
    }) crossBuilds;

    shellSetup = map (config: ''
      alias ${config}-ghc=${crossSystems."${config}".pkgs.stdenv.targetPlatform.config}-ghc
      alias ${config}-cabal=${crossSystems."${config}".pkgs.stdenv.targetPlatform.config}-cabal
    '') crossBuilds;
    default = project.shellFor {
      packages = ps: __shells__ ps;
      inherit withHoogle exactDeps;
      tools = shellTools;
      inputsFrom = crossProjects;
      shellHook = builtins.concatStringsSep "\n" shellSetup;
    };
    cross = crossSystems."${targetSystem}".shellFor {
      packages = ps: __shells__ ps;
      inherit withHoogle exactDeps;
      tools = shellTools;
      shellHook = builtins.concatStringsSep "\n" shellSetup;
    };
  in if !(justCross) then default else cross;

in shellDef shellArgs
