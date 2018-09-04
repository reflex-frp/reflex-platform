{ this }:

let
  skeletonSrc = this.nixpkgs.fetchFromGitHub {
    owner = "ElvishJerricco";
    repo = "reflex-project-skeleton";
    rev = "27d6181af3cd77b04d5e5d441b31c04421215338";
    sha256 = "1lv52mbmgrvr4jan1axy7kxy4kx51p1rzchx71rf6zg22zlvcdla";
    fetchSubmodules = false; # Not interested in its reflex-platform checkout
  };

  skeleton = import skeletonSrc { reflex-platform = this; };

  mkCabalProject = { shellDrv, projectFile }: shellDrv.overrideAttrs (old: {
    name = "reflex-project-skeleton-${projectFile}";
    src = skeletonSrc;
    CABAL_CONFIG = builtins.toFile "cabal.config" ''
    '';
    buildPhase = ''
      HOME=$NIX_BUILD_TOP
      cabal new-build all --project-file=${projectFile}
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
