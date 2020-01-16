{ haskellLib
, enableLibraryProfiling
}:

with haskellLib;

self: super: {

  # Override mkDerivation to inherit global settings
  mkDerivation = expr: super.mkDerivation (expr // {
    inherit enableLibraryProfiling;
  });
}
