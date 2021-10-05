{ lib }:

self: super:

{
  ghc = super.ghc.overrideAttrs (drv: {
    patches = (drv.patches or []) ++ [ ./fast-weak.patch ];
    phases = [ "unpackPhase" "patchPhase" "buildPhase" ];
  });
}
