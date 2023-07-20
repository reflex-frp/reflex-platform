{ shells ? (ps: []),
  project,
  crossSystems,
  withHoogle,
  shellTools,
  exactDeps,
  unsafeMsg,
  ...
}@top: shellArgs:

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
    buildInputs ? [ ],
    packages ? {},
    srcdir ? "",
    additional ? (p: with p; [ ]),
    shellTools ? top.shellTools or {},
    exactDeps ? top.exactDeps or false,
    ...
  }@args: let
    crossProjects = map (a: crossSystems."${a}".shellFor {
      packages = ps: __shells__ ps;
      withHoogle = false;
    }) crossBuilds;

    # We do this since we expect haskell.nix to solve our project for use and provide correct deps
    # we can get away with this. Otherwise our "packages" attribute in nix would completely break everything
    shellCabalProject = builtins.toFile "cabal.shell.project" ''
      packages: ${builtins.replaceStrings ["./"] ["${srcdir}/"] (builtins.concatStringsSep " "  (builtins.attrValues packages))}
    '';

    wrapCabal = ''
      export CABAL_PROJECT=${shellCabalProject}
      alias cabal="cabal --project-file=${shellCabalProject} --builddir=${srcdir}/dist-newstyle"
    '';

    shellSetup = map (config: ''
      alias ${config}-ghc=${crossSystems."${config}".pkgs.stdenv.targetPlatform.config}-ghc
      alias ${config}-cabal=${crossSystems."${config}".pkgs.stdenv.targetPlatform.config}-cabal
    '') crossBuilds;
    default = project.shellFor {
      packages = ps: __shells__ ps;
      inherit withHoogle exactDeps buildInputs;
      tools = shellTools;
      inputsFrom = crossProjects;
      shellHook = wrapCabal + (builtins.concatStringsSep "\n" shellSetup);
      inherit additional;
    };
    cross = crossSystems."${targetSystem}".shellFor {
      packages = ps: __shells__ ps;
      inherit withHoogle exactDeps buildInputs;
      tools = shellTools;
      shellHook = builtins.concatStringsSep "\n" shellSetup;
    };
  in if !(justCross) then default else cross;

in shellDef shellArgs
