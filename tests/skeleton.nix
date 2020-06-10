{ reflex-platform }:

let
  skeletonSrc = reflex-platform.nixpkgs.fetchFromGitHub (builtins.fromJSON (builtins.readFile ../examples/project/reflex-project-skeleton/github.json))
  // {
    fetchSubmodules = false; # Not interested in its reflex-platform checkout
  };

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
in {
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
