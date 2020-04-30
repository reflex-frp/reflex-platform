self: super: {
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

