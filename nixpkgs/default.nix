let
  fetchFromGitHub = { owner, repo, rev, sha256, branch }:
    if (builtins ? "fetchTarball")
    then builtins.fetchTarball {
        inherit sha256;
        url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
      }
    else (import <nixpkgs> {}).fetchFromGitHub { inherit owner repo rev; };
in import (fetchFromGitHub (builtins.fromJSON (builtins.readFile ./github.json)))
