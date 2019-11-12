# DO NOT HAND-EDIT THIS FILE
let fetchGit = {url, rev, ref ? null, branch ? null, sha256 ? null, fetchSubmodules ? null}:
  assert !fetchSubmodules; (import <nixpkgs> {}).fetchgit { inherit url rev sha256; };
in import (fetchGit (builtins.fromJSON (builtins.readFile ./git.json)))
