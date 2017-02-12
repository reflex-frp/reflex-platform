import ((import <nixpkgs> {}).fetchFromGitHub (builtins.fromJSON (builtins.readFile ./github.json)))
#import /home/john/Code/git/nixpkgs-reflex
