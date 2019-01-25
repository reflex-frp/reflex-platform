# DO NOT HAND-EDIT THIS FILE
let
  fetch = { private ? false, ... }@args:
    if private && builtins.hasAttr "fetchGit" builtins
    then fetchFromGitHubPrivate args
    else fetchFromGitHub (builtins.removeAttrs args ["branch"]);
  fetchFromGitHub = if builtins.hasAttr "fetchTarball" builtins then
    ({ owner, repo, rev, sha256, ... }: builtins.fetchTarball {
      url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
      inherit sha256; })
    else (import <nixpkgs> {}).fetchFromGitHub;
  fetchFromGitHubPrivate =
    { owner, repo, rev, branch ? null, name ? null, sha256 ? null
    , private ? false, fetchSubmodules ? false
    , githubBase ? "github.com", ...}: assert !fetchSubmodules;
      builtins.fetchGit ({
        url = "ssh://git@${githubBase}/${owner}/${repo}.git";
        inherit rev;
      } // (if branch == null then {} else { ref = branch; })
        // (if name == null then {} else { inherit name; }));
in import (fetch (builtins.fromJSON (builtins.readFile ./github.json)))
