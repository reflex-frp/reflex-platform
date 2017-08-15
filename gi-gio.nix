{ mkDerivation, base, bytestring, Cabal, containers, gi-glib
, gi-gobject, glib, haskell-gi, haskell-gi-base
, haskell-gi-overloading, stdenv, text, transformers
}:
mkDerivation {
  pname = "gi-gio";
  version = "2.0.14";
  sha256 = "0dwy8zd66b04jbn0g7c5n511nl2xxjvchzf56bmw8cfcm384r66d";
  setupHaskellDepends = [ base Cabal haskell-gi ];
  libraryHaskellDepends = [
    base bytestring containers gi-glib gi-gobject haskell-gi
    haskell-gi-base haskell-gi-overloading text transformers
  ];
  libraryPkgconfigDepends = [ glib ];
  homepage = "https://github.com/haskell-gi/haskell-gi";
  description = "Gio bindings";
  license = stdenv.lib.licenses.lgpl21;
}
