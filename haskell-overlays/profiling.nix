{ haskellLib
, enableLibraryProfiling
}:

with haskellLib;

self: super: {

  _cantProfile = {};

  # Override mkDerivation to inherit global settings
  mkDerivation = expr: super.mkDerivation (expr // {
    enableLibraryProfiling = enableLibraryProfiling && !self._cantProfile ? ${expr.pname};
  });
}
