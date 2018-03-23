import ((import <nixpkgs> {}).fetchFromGitHub (builtins.fromJSON (builtins.readFile ./github.json)))
