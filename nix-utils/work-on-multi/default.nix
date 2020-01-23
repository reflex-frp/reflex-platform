{ reflex-platform ? import ../.. { hideDeprecated = false; } }:

let
  inherit (reflex-platform)
    nixpkgs
    ghc
    overrideCabal
    generalDevTools'
    ;
  inherit (nixpkgs) lib;
in

{ envFunc, packageNames, tools ? _: [], shellToolOverrides ? _: _: {} }:

let
  inherit (builtins) listToAttrs filter attrValues all concatLists;
    combinableAttrs = p: [
      "buildDepends"
      "buildTools"
      "executableFrameworkDepends"
      "executableHaskellDepends"
      "executablePkgconfigDepends"
      "executableSystemDepends"
      "executableToolDepends"
      "extraLibraries"
      "libraryFrameworkDepends"
      "libraryHaskellDepends"
      "libraryPkgconfigDepends"
      "librarySystemDepends"
      "libraryToolDepends"
      "pkgconfigDepends"
      "setupHaskellDepends"
    ] ++ lib.optionals (p.doCheck or true) [
      "testDepends"
      "testFrameworkDepends"
      "testHaskellDepends"
      "testPkgconfigDepends"
      "testSystemDepends"
      "testToolDepends"
    ] ++ lib.optionals (p.doBenchmark or false) [
      "benchmarkDepends"
      "benchmarkFrameworkDepends"
      "benchmarkHaskellDepends"
      "benchmarkPkgconfigDepends"
      "benchmarkSystemDepends"
      "benchmarkToolDepends"
    ];

    concatCombinableAttrs = haskellConfigs: lib.filterAttrs
      (n: v: v != [])
      (lib.zipAttrsWith (_: concatLists) (map
        (haskellConfig: lib.listToAttrs (map
          (name: {
            inherit name;
            value = haskellConfig.${name} or [];
          })
          (combinableAttrs haskellConfig)))
        haskellConfigs
        ));

    getHaskellConfig = p: (overrideCabal p (args: {
      passthru = (args.passthru or {}) // {
        out = args;
      };
    })).out;
    notInTargetPackageSet = p: all (pname: (p.pname or "") != pname) packageNames;
    baseTools = generalDevTools' {};
    env = envFunc reflex-platform;
    overriddenTools = baseTools // shellToolOverrides env baseTools;
    depAttrs = lib.mapAttrs (_: v: filter notInTargetPackageSet v) (concatCombinableAttrs (concatLists [
      (map getHaskellConfig (lib.attrVals packageNames env))
      [{
        buildTools = [
          (nixpkgs.buildEnv {
            name = "build-tools-wrapper";
            paths = attrValues overriddenTools ++ tools env;
            pathsToLink = [ "/bin" ];
            extraOutputsToInstall = [ "bin" ];
          })
          overriddenTools.Cabal
        ];
      }]
    ]));

in (env.mkDerivation (depAttrs // {
  pname = "work-on-multi--combined-pkg";
  version = "0";
  license = null;
})).env
