{ reflex-platform }:

let
  skeletonSrc = reflex-platform.nixpkgs.fetchFromGitHub {
    owner = "ElvishJerricco";
    repo = "reflex-project-skeleton";
    rev = "d1cf6b26a9aa08b192e3e81ae07a4ba00064d6d2";
    sha256 = "05a3rq4ar77fpwl05z7niz025lp5wrwxzzz804jvwkamvyjxsyf2";
    fetchSubmodules = false; # Not interested in its reflex-platform checkout
  };

  skeleton = import skeletonSrc { inherit reflex-platform; };

  mkCabalProject = { shellDrv, projectFile }: shellDrv.overrideAttrs (old: {
    name = "reflex-project-skeleton-${projectFile}";
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
  project = skeleton.all;
  ghc = mkCabalProject {
    shellDrv = skeleton.shells.ghc;
    projectFile = "cabal.project";
  };
  ghcjs = mkCabalProject {
    shellDrv = skeleton.shells.ghcjs;
    projectFile = "cabal-ghcjs.project";
  };
}
