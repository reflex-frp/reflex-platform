{ reflex-platform }:

let
  skeletonSrc = reflex-platform.hackGet ../examples/project/reflex-project-skeleton;

  skeleton = import skeletonSrc { inherit reflex-platform; };

  mkCabalProject = { shellDrv, projectFile }: shellDrv.overrideAttrs (old: {
    name = "reflex-project-skeleton-${projectFile}";
    phases = [ "unpackPhase" "buildPhase" "installPhase" ];
    src = skeletonSrc;
    CABAL_CONFIG = builtins.toFile "cabal.config" ''
    '';
    buildPhase = ''
      HOME=$NIX_BUILD_TOP
      cabal new-build all --project-file=${projectFile} ${if reflex-platform.nixpkgs.stdenv.isDarwin then "--ghc-option=-dynamic" else ""}
    '';
    installPhase = ''
      mv ./dist-newstyle $out
    '';
  });
in
{
  project = skeleton;
  ghc = mkCabalProject {
    shellDrv = skeleton.shells.ghc;
    projectFile = "cabal.project";
  };
  ghcjs = mkCabalProject {
    shellDrv = skeleton.shells.ghcjs;
    projectFile = "cabal-ghcjs.project";
  };
}
