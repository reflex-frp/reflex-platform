{ haskellLib
, enableLibraryProfiling
}:

with haskellLib;

self: super: {
  optparse-applicative = haskellLib.overrideCabal super.optparse-applicative (drv: {
    enableLibraryProfiling = false;
  });
  th-orphans = haskellLib.overrideCabal super.th-orphans (drv: {
    enableLibraryProfiling = false;
  });

  # Override mkDerivation to inherit global settings
  mkDerivation = expr: super.mkDerivation (expr // {
    inherit enableLibraryProfiling;
  });
}
