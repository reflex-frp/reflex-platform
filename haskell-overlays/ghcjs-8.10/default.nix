{ lib, haskellLib, nixpkgs, fetchgit, fetchFromGitHub
, useReflexOptimizer
, useTextJSString
, enableLibraryProfiling
}:

with haskellLib;

self: super: {
  text-short = dontCheck super.text-short;
  ghcjs-base = dontCheck (self.callHackage "ghcjs-base" "0.2.1.0" {});
}
