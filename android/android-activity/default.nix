{ mkDerivation, base, data-default, jdk, stdenv }:
mkDerivation {
  pname = "android-activity";
  version = "0.1";
  src = builtins.filterSource (path: type: !(builtins.any (x: x == baseNameOf path) [".git"])) ./.;
  libraryHaskellDepends = [ base data-default ];
  librarySystemDepends = [ jdk ];
  homepage = "https://github.com/obsidiansystems/android-activity";
  description = "Turn regular Haskell programs into Android Activities";
  license = stdenv.lib.licenses.bsd3;
}
