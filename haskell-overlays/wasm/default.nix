{ haskellLib
, thunkSet
}:
with haskellLib;
let
  localDeps = thunkSet ./dep;
in self: super: {
  # _dep = super._dep or {} // localDeps;
  jsaddle-warp = null;
  jsaddle-webkitgtk = null;
  jsaddle-webkit2gtk = null;
  jsaddle-wkwebview = null;
  mkDerivation = args: super.mkDerivation (args // {
    dontStrip = true;
    enableSharedExecutables = false;
    enableSharedLibraries = false;
    enableDeadCodeElimination = false;
    doHaddock = false;
    doCheck = false;
    enableLibraryProfiling = false;
  });
}

