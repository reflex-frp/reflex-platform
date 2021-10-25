{ pkgs, haskellLib, fetchFromGitHub }:
with haskellLib;
self: super: {
  HUnit = dontCheck super.HUnit;
}
