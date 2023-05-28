{ inputMap, thunkSource, pkgs, ... }: let

  readJSON = x: builtins.fromJSON (builtins.readFile x);

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

  checkFor = x: dir: {
    "gitlab" = builtins.readDir dir ? "gitlab.json";
    "github" = builtins.readDir dir ? "github.json";
    "git" = builtins.readDir dir ? "git.json";
    "gitdir" = builtins.readDir dir ? ".git";
  }."${x}";

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
      json = readUnpacked v;
    in unpackedParser v json;
  }."${x}";

  # jsonReader :: FilePath -> "Text"
  jsonReader = v: if checkFor "github" v then
    "github"
  else if checkFor "gitlab" v then
    "gitlab"
  else if checkFor "git" v then
    "git"
  else if checkFor "gitdir" v then
    "unpacked"
  else builtins.error "Not a thunk!";

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

  parser = list: builtins.listToAttrs (builtins.concatMap (a: finalParse a) list);

  cabalWithSubDirs = val: let
    reader = jsonReader val.thunk;
    parsed = parseFor reader val.thunk;
  in builtins.map (a: ''
    source-repository-package
      type: git
      location: ${parsed.value.url}/${a}
      tag: ${parsed.value.rev}
  '') val.subdirs;

  cabalProjectGen = val: let
    reader = jsonReader val;
    parsed = parseFor reader val;
  in [''
    source-repository-package
      type: git
      location: ${parsed.value.url}
      tag: ${parsed.value.rev}
  ''];

  genOverridesForSubdirs = val: let
    source = thunkSource val.thunk;
  in map (a:
    ({config, pkgs, lib, ... }: {
      packages."${a}".src = lib.mkForce source;
    })) val.subdirs;

in {
  inputMap = parser inputMap;
  cabalProject = builtins.concatMap (a: if a ? subdirs then cabalWithSubDirs a else cabalProjectGen a) inputMap;
  overrides = [];
  #builtins.concatMap (a: if a ? subdirs then genOverridesForSubdirs a else []) inputMap;
}
