{ mkDerivation, base, bytestring, Cabal, containers, glib
, haskell-gi, haskell-gi-base, haskell-gi-overloading, stdenv, text
, transformers
}:
mkDerivation {
  pname = "gi-glib";
  version = "2.0.14";
  sha256 = "09bmrrppbjcy90f66qdhsj2hz3x7ci80mrdgryajzqrin4zs7aqx";
  setupHaskellDepends = [ base Cabal haskell-gi ];
  libraryHaskellDepends = [
    base bytestring containers haskell-gi haskell-gi-base
    haskell-gi-overloading text transformers
  ];
  libraryPkgconfigDepends = [ glib ];
  homepage = "https://github.com/haskell-gi/haskell-gi";
  description = "GLib bindings";
  license = stdenv.lib.licenses.lgpl21;
}
