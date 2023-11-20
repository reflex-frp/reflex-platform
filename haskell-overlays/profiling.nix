{ haskellLib
, enableLibraryProfiling
}:

with haskellLib;

let
  preventMobileProfiling = (!self.ghc.stdenv.targetPlatform.isiOS) && enableLibraryProfiling;
in

self: super: {

  # Override mkDerivation to inherit global settings
  mkDerivation = expr: super.mkDerivation (expr // {
    enableLibraryProfiling = preventMobileProfiling;
  });
}
