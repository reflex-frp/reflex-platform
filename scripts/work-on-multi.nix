{ reflex-platform ? import ../. {} }:

let
  inherit (reflex-platform)
    nixpkgs
    ghc
    lib
    overrideCabal
    ;
in

{ env, packageNames, tools ? _: [], shellToolOverrides ? _: _: {} }:
  let inherit (builtins) listToAttrs filter attrValues all concatLists;
      combinableAttrs = [
        "benchmarkDepends"
        "benchmarkFrameworkDepends"
        "benchmarkHaskellDepends"
        "benchmarkPkgconfigDepends"
        "benchmarkSystemDepends"
        "benchmarkToolDepends"
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
        "testDepends"
        "testFrameworkDepends"
        "testHaskellDepends"
        "testPkgconfigDepends"
        "testSystemDepends"
        "testToolDepends"
      ];
      concatCombinableAttrs = haskellConfigs: lib.filterAttrs (n: v: v != []) (lib.listToAttrs (map (name: { inherit name; value = concatLists (map (haskellConfig: haskellConfig.${name} or []) haskellConfigs); }) combinableAttrs));
      getHaskellConfig = p: (overrideCabal p (args: {
        passthru = (args.passthru or {}) // {
          out = args;
        };
      })).out;
      notInTargetPackageSet = p: all (pname: (p.pname or "") != pname) packageNames;
      baseTools = generalDevToolsAttrs env;
      overriddenTools = attrValues (baseTools // shellToolOverrides env baseTools);
      depAttrs = lib.mapAttrs (_: v: filter notInTargetPackageSet v) (concatCombinableAttrs (concatLists [
        (map getHaskellConfig (lib.attrVals packageNames env))
        [{
          buildTools = overriddenTools ++ tools env;
        }]
      ]));

  in (env.mkDerivation (depAttrs // {
    pname = "work-on-multi--combined-pkg";
    version = "0";
    license = null;
  })).env
