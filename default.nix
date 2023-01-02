import ./project.nix {
  name = "reflex-todomvc";
  src = ../reflex-todomvc;
  compiler-nix-name = "ghc8107Splices";
  extraSrcFiles = {
    library.extraSrcFiles = [ "style.css" ];
    exes.reflex-todomvc.extraSrcFiles = [ "style.css" ];
  };
  overrides = [
    { packages.reflex.configureFlags = [ "-f-use-template-haskell" ]; }
    ({ config, lib, ... }: { packages.bitvec.patches = lib.mkForce [ ]; })
  ];
}
