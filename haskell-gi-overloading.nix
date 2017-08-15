{ mkDerivation, stdenv }:
mkDerivation {
  pname = "haskell-gi-overloading";
  version = "0.0";
  sha256 = "1smz5fr5saw1l129h21qcywyp47mrbf7355kmwkspjh75yl2gix5";
  doHaddock = false;
  homepage = "https://github.com/haskell-gi/haskell-gi";
  description = "Overloading support for haskell-gi";
  license = stdenv.lib.licenses.bsd3;
}
