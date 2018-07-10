{ haskellLib, fetchFromGitHub }:

self: super: {
  # th-expand-syns = haskellLib.doJailbreak super.th-expand-syns;
  # ChasingBottoms = haskellLib.doJailbreak super.ChasingBottoms;
  # base-orphans = haskellLib.dontCheck super.base-orphans;
  # bifunctors = haskellLib.dontCheck super.bifunctors;
  # HTTP = haskellLib.doJailbreak super.HTTP;
  # newtype-generics = haskellLib.doJailbreak super.newtype-generics;

  # extra = haskellLib.replaceSrc super.extra (fetchFromGitHub {
  #   owner = "ndmitchell";
  #   repo = "extra";
  #   rev = "22b0e6aa6077b2d969e8b8ac613f5a3455d9e88d";
  #   sha256 = "0milbw2azkj22rqacrnd0x4wh65qfrl3nhbmwfxzmdrsc2la3bkh";
  # }) "1.5.2";

  integer-logarithms = haskellLib.doJailbreak super.integer-logarithms;
  tagged = self.callHackage "tagged" "0.8.6" {};
  vector = haskellLib.doJailbreak super.vector;
  th-abstraction = self.callHackage "th-abstraction" "0.2.8.0" {};

  stm = haskellLib.overrideCabal super.stm (drv: {
    src = fetchFromGitHub {
      owner = "haskell";
      repo = "stm";
      rev = "c107caefc08606f231dd6e8b9e0f1295e44bd846";
      sha256 = "07m4bkizsbv2gclrydja3dkjjgyhaqnzgh9zfsp9dm5y7hz8xlj9";
    };
  });


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
      if [ -d $(pwd)/src ]; then
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
