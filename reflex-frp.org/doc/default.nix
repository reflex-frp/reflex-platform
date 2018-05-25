{ stdenv, pythonPackages }:

stdenv.mkDerivation {
  name = "reflex-readthedocs";
  src = ./.;
  buildFlags = ["dirhtml"];
  nativeBuildInputs = [pythonPackages.sphinx];
  installPhase = "mv _build $out";
}
