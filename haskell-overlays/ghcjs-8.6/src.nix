{ fetchgit, mkDerivation }:
let
  rawSrc = fetchgit {
    url = "https://github.com/obsidiansystems/ghcjs.git";
    rev = "a00ecf0b2eaddbc4101c76e6ac95fc97b0f75840"; # ghc-8.6 branch
    sha256 = "06cwpijwhj4jpprn07y3pkxmv40pwmqqw5jbdv4s7c67j5pmirnc";
    fetchSubmodules = true;
  };
  # See https://gitlab.haskell.org/ghc/ghc/-/commit/ad2ef3a13f1eb000eab8e3d64592373b91a52806
  autoreconfPatch = ./autoreconf.patch;
  patchedSrc = mkDerivation {
    name = "ghcjs-src-autoreconf-patched";
    src = rawSrc;
    buildPhase = ''
      cp -r $src $out
      chmod -R +w $out/*
      cd $out
      patch -p1 < ${autoreconfPatch}
    '';
    dontInstall = true;
  };
in
patchedSrc
