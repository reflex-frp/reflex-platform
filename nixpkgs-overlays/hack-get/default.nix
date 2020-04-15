{ lib }:

self:

{
  filterGit = builtins.filterSource (path: type: !(builtins.any (x: x == baseNameOf path) [".git" "tags" "TAGS" "dist"]));

  # Retrieve source that is controlled by the hack-* scripts; it may be either a stub or a checked-out git repo
  hackGet = p:
    let
      contents = builtins.readDir p;

      contentsMatch = { required, optional }:
           (let all = required // optional; in all // contents == all)
        && builtins.intersectAttrs required contents == required;

      # Newer obelisk thunks include the feature of hackGet with a thunk.nix file in the thunk.
      isObeliskThunkWithThunkNix =
        let
          common = {
            required = { "default.nix" = "regular"; "thunk.nix" = "regular"; };
            optional = { ".attr-cache" = "directory"; };
          };
          packed = jsonFileName: {
            required = { ${jsonFileName} = "regular"; } // common.required;
            optional = common.optional;
          };
          unpacked = {
            required = { "local" = "directory"; } // common.required;
            optional = common.optional;
          };
        in contentsMatch unpacked
          || builtins.any (n: contentsMatch (packed n)) [ "git.json" "github.json" ];

      filterArgs = x: removeAttrs x [ "branch" ];
      hasValidThunk = name: if builtins.pathExists (p + ("/" + name))
        then
          contentsMatch {
            required = { ${name} = "regular"; };
            optional = { "default.nix" = "regular"; ".attr-cache" = "directory"; };
          }
          || throw "Thunk at ${toString p} has files in addition to ${name} and optionally default.nix and .attr-cache. Remove either ${name} or those other files to continue (check for leftover .git too)."
        else false;
    in
      if isObeliskThunkWithThunkNix then
        builtins.trace
          "DEPRECATED: hackGet is deprecated for thunks containing thunk.nix; Use `import ${p}/thunk.nix` instead of `hackGet ${p}`"
          (import (p + /thunk.nix))
      else if hasValidThunk "git.json" then (
        let gitArgs = filterArgs (builtins.fromJSON (builtins.readFile (p + "/git.json")));
        in if builtins.elem "@" (lib.stringToCharacters gitArgs.url)
          then self.fetchgitPrivate gitArgs
          else self.fetchgit gitArgs
        )
      else if hasValidThunk "github.json" then
        self.fetchFromGitHub (filterArgs (builtins.fromJSON (builtins.readFile (p + "/github.json"))))
      else {
        name = baseNameOf p;
        outPath = self.filterGit p;
      };

  # Make an attribute set of source derivations for a directory containing thunks:
  thunkSet = dir: lib.mapAttrs (name: _: self.hackGet (dir + "/${name}")) (builtins.readDir dir);
}
