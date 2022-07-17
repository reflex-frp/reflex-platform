{ lib, haskellLib, nixpkgs, fetchgit, fetchFromGitHub
, useReflexOptimizer
, useTextJSString
, enableLibraryProfiling
}:

with haskellLib;

self: super: {
  text-short = dontCheck super.text-short;
}
