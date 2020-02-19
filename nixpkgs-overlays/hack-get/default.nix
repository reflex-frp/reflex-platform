{ lib }:

self:

{
  filterGit = builtins.filterSource (path: type: !(builtins.any (x: x == baseNameOf path) [".git" "tags" "TAGS" "dist"]));

  # Retrieve source that is controlled by the hack-* scripts; it may be either a stub or a checked-out git repo
  hackGet = p:
    let
      filterArgs = x: removeAttrs x [ "branch" ];
      hasValidThunk = name: if builtins.pathExists (p + ("/" + name))
        then (let contents = builtins.readDir p;
              in if contents == { ${name} = "regular"; } || contents == { ${name} = "regular"; "default.nix" = "regular"; }
                 then true
                 else throw "Thunk at ${toString p} has files in addition to ${name} and default.nix. Remove either ${name} or those other files to continue.")
        else false;
    in if hasValidThunk "git.json" then (
      let gitArgs = filterArgs (builtins.fromJSON (builtins.readFile (p + "/git.json")));
      in if builtins.elem "@" (lib.stringToCharacters gitArgs.url)
      then self.fetchgitPrivate gitArgs
      else self.fetchgit gitArgs)
    else if hasValidThunk "github.json" then
      self.fetchFromGitHub (filterArgs (builtins.fromJSON (builtins.readFile (p + "/github.json"))))
    else {
      name = baseNameOf p;
      outPath = self.filterGit p;
    };

  # Make an attribute set of source derivations for a directory containing thunks:
  thunkSet = dir: lib.mapAttrs (name: _: self.hackGet (dir + "/${name}")) (builtins.readDir dir);
}
