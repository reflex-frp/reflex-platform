{ nixpkgs }:

rec {
  filterGit = builtins.filterSource (path: type: !(builtins.any (x: x == baseNameOf path) [".git" "tags" "TAGS" "dist"]));

  # Retrieve source that is controlled by the hack-* scripts; it may be either a stub or a checked-out git repo
  hackGet = p:
    let filterArgs = x: removeAttrs x [ "branch" ];
    in if builtins.pathExists (p + "/git.json") then (
      let gitArgs = filterArgs (builtins.fromJSON (builtins.readFile (p + "/git.json")));
      in if builtins.elem "@" (nixpkgs.lib.stringToCharacters gitArgs.url)
      then nixpkgs.fetchgitPrivate gitArgs
      else nixpkgs.fetchgit gitArgs)
    else if builtins.pathExists (p + "/github.json") then nixpkgs.fetchFromGitHub (filterArgs (builtins.fromJSON (builtins.readFile (p + "/github.json"))))
    else {
      name = baseNameOf p;
      outPath = filterGit p;
    };

  # Make an attribute set of source derivations for a directory containing thunks:
  thunkSet = dir: nixpkgs.lib.mapAttrs (name: _: hackGet (dir + "/${name}")) (builtins.readDir dir);
}
