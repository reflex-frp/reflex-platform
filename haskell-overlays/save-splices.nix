{ haskellLib, fetchFromGitHub, lib }:

self: super: {
  # -save-splices assumes that all of the sources are in the same
  # -directory. Aeson has two directories: ./. & attoparsec-iso8601.
  # The only way I know how to fix this is to just copy everything to
  # the same directory.
  aeson = haskellLib.overrideCabal super.aeson_1_4_0_0 (drv: {
    postConfigure = ''
      (cd attoparsec-iso8601 && find . -type f -exec install -D '{}' "$spliceDir/{}" \;)
    '';
  });

  mkDerivation = drv: super.mkDerivation (drv // {

    # ANN is unimplemented in splices.patch
    postPatch = ''
      ${drv.postPatch or ""}
      find . -name '*.hs' -exec sed -i 's/^{-# ANN .* #-}$//' '{}' \;
    '';

    # We need to find the correct directory to dump splices into. This
    # may be possible through cabal? The directory is usually ./. or
    # ./src
    preConfigure = ''
      ${drv.preConfigure or ""}
      if [ "${drv.pname}" = "reflection" ]; then
        spliceDir="$(pwd)/fast"
      elif [ -d $(pwd)/lib ]; then
        spliceDir="$(pwd)/lib"
      elif [ -d $(pwd)/src ]; then
        spliceDir="$(pwd)/src"
      else
        spliceDir="$(pwd)"
      fi
      configureFlags+=" --ghc-option=-ddump-splices"
      configureFlags+=" --ghc-option=-save-splices=$spliceDir"
    '';

    # Install the splices into $out/lib/ghc-8.5/<name>-<version>/
    # These will be loaded in cross compilation.
    postInstall = ''
      ${drv.postInstall or ""}
      (cd $spliceDir && find . -name '*.hs-splice' -exec install -D '{}' "$out/lib/ghc-8.5/${drv.pname}-${drv.version}/{}" \;)
    '';

    # Disable a few things that are broken wtih splices.patch
    hyperlinkSource = false;
    doCheck = false;
  });
}
