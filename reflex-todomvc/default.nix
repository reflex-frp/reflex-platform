{ mkDerivation, reflex, reflex-dom, file-embed
}:

mkDerivation {
  pname = "reflex-todomvc";
  version = "0.1";
  src = builtins.filterSource (path: type: baseNameOf path != ".git") ./.;
  isExecutable = true;
  isLibrary = true;
  buildDepends = [
    reflex
    reflex-dom
    file-embed
  ];
  license = null;
}
