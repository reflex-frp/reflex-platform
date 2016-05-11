import ((import <nixpkgs> {}).fetchgit (builtins.fromJSON (builtins.readFile ./git.json)))
