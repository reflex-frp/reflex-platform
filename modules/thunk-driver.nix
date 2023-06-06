{ inputMap, thunkSource, pkgs, ... }: let

  readJSON = x: builtins.fromJSON (builtins.readFile x);

  # NOTE: Recursive hash function that produces reliable hashes of the directory
  # This has been tested to be reliable

  # REVIEW: Check if we actually want to do it this way
  # hashDirRec :: [FilePath] -> [String] -> [String]
  hashDirRec = { path, exclude ? [] }: let
    dir = builtins.removeAttrs (builtins.readDir path) exclude;
  in  builtins.concatMap (a:
    if dir.${a} == "directory" then
      hashDirRec {
        path = (path + "/${a}");
        inherit exclude;
      }
    else if dir.${a} == "regular" then
      [ (builtins.hashFile "sha256" (path + "/${a}")) ]
    else [ "nohash" ]
    ) (builtins.attrNames dir);

  githubParser = v: json: {
    name = "https://github.com/${json.owner}/${json.repo}/${json.rev}";
    value = {
      srcPath = thunkSource v;
      url = "https://github.com/${json.owner}/${json.repo}";
      rev = json.rev;
    };
  };

  gitlabParser = v: json: {
    name = "https://gitlab.com/${json.owner}/${json.repo}/${json.rev}";
    value = {
      srcPath = thunkSource v;
      url = "https://gitlab.com/${json.owner}/${json.repo}";
      rev = json.rev;
    };
  };

  gitParser = v: json: {
    name = "${json.url}/${json.rev}";
    value = {
      srcPath = thunkSource v;
      url = json.url;
      rev = json.rev;
    };
  };

  unpackedParser = v: json: {
    name = "${json.url}/${json.rev}";
    value = {
      srcPath = (thunkSource v).outPath;
      url = json.url;
      rev = json.rev;
      thunk_unpacked = v;
    };
  };

  getRev = v: pkgs.runCommandNoCC "getRev" {  } ''
    cd ${v}
    mkdir -p $TMPDIR
    export HOME=$TMPDIR

    mkdir -p $out
    ${pkgs.git}/bin/git config --global --add safe.directory ${v}
    ${pkgs.git}/bin/git rev-parse HEAD | tr -d '\n' > $out/rev
    ${pkgs.git}/bin/git remote get-url origin | tr -d '\n' | sed 's/\.git//g' > $out/url
  '';

  readUnpacked = v: let
    gitThings = getRev v;
    rev = builtins.readFile "${gitThings}/rev";
    url = builtins.readFile "${gitThings}/url";
  in {
    inherit url rev;
  };

  # hashContentsForAttrs :: FilePath -> AttrSet
  hashContentsForAttrs = val: let
    hashedDir = builtins.concatStringsSep "" (hashDirRec {
      path = val;
      exclude = [ ".git" ".gitignore" ];
    });
  in {
    name = builtins.hashString "sha256" hashedDir;
    rev = builtins.hashString "sha1" hashedDir;
  };

  # checkFor :: String -> FilePath -> Bool
  checkFor = x: dir: {
    "gitlab" = builtins.readDir dir ? "gitlab.json";
    "github" = builtins.readDir dir ? "github.json";
    "git" = builtins.readDir dir ? "git.json";
    "gitdir" = builtins.readDir dir ? ".git";
  }."${x}";

  # TODO: Maybe we want all of this to use hashContentsForAttrs instead of relying on git rev
  # parseFor :: String -> FilePath -> AttrSet
  parseFor = x: v: {
    "gitlab" = let
      json = readJSON (v + "/gitlab.json");
    in gitlabParser v json;
    "github" = let
      json = readJSON (v + "/github.json");
    in githubParser v json;
    "git" = let
      json = readJSON (v + "/git.json");
    in gitParser v json;
    "unpacked" = let
      generated_attrs = hashContentsForAttrs v;
      json = {
        url = "https://fake.git/lib/${generated_attrs.name}";
        rev = "${generated_attrs.rev}";
      };
    in unpackedParser v json;
    "nonthunk" = let
      generated_attrs = hashContentsForAttrs v;
      json = {
        url = "https://fake.git/lib/${generated_attrs.name}";
        rev = "${generated_attrs.rev}";
      };
    in unpackedParser v json;
  }."${x}";

  # jsonReader :: FilePath -> String
  jsonReader = v: if checkFor "github" v then
    "github"
  else if checkFor "gitlab" v then
    "gitlab"
  else if checkFor "git" v then
    "git"
  else if checkFor "gitdir" v then
    "unpacked"
  else "nonthunk";

  # finalParse :: Either AttrSet FilePath -> [AttrSet]
  finalParse = v: if (v ? subdirs) then (map (a: let
    reader = jsonReader v.thunk;
    parsed = parseFor reader v.thunk;
  in {
    name = "${parsed.value.url}/${a}/${parsed.value.rev}";
    value = (parsed.value.thunk_unpacked or parsed.value.srcPath) + "/${a}";
  }) v.subdirs)
  else (let
    reader = jsonReader v;
    parsed = parseFor reader v;
  in [{
    inherit (parsed) name;
    value = parsed.value.srcPath;
  }]);

  # parser :: [Either AttrSet FilePath] -> AttrSet
  parser = list: builtins.listToAttrs (builtins.concatMap (a: finalParse a) list);

  # cabalWithSubDirs :: AttrSet -> [String]
  cabalWithSubDirs = val: let
    reader = jsonReader val.thunk;
    parsed = parseFor reader val.thunk;
  in builtins.map (a: ''
    source-repository-package
      type: git
      location: ${parsed.value.url}/${a}
      tag: ${parsed.value.rev}
  '') val.subdirs;

  # cabalProjectGen :: FilePath -> [String]
  cabalProjectGen = val: let
    reader = jsonReader val;
    parsed = parseFor reader val;
  in [''
    source-repository-package
      type: git
      location: ${parsed.value.url}
      tag: ${parsed.value.rev}
  ''];

in {
  inputMap = parser inputMap;
  cabalProject = builtins.concatMap (a: if a ? subdirs then cabalWithSubDirs a else cabalProjectGen a) inputMap;
  overrides = [];
  #builtins.concatMap (a: if a ? subdirs then genOverridesForSubdirs a else []) inputMap;
}
