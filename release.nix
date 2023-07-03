{ supportedSystems ? [ "x86_64-linux"  "x86_64-darwin" ], ... }:
let
  project = import ./default.nix { };
  pkgs = project.pkgs;
  systems = pkgs.lib.genAttrs supportedSystems;

  allAttrs = systems (system: let
    example = import ./example/default.nix { inherit system; };
    tests = import ./tests/thunk_tests.nix { inherit system; };
  in {
    recurseForDerivations = true;
    ghcjs-app = example.ghcjs-app;
    app = example.hsPkgs.reflex-todomvc.components.exes.reflex-todomvc;
    ghcShell = example.shells.ghc;
    inherit (tests) testOut;
  } // pkgs.lib.optionalAttrs (system == "x86_64-linux") {
    android-app = example.android.app.aarch64;
  } // pkgs.lib.optionalAttrs (system == "x86_64-darwin") {
    ios-app = example.ios.app.aarch64;
  });
in allAttrs // {
  recurseForDerivations = true;
  ghc8107 = pkgs.haskell-nix.compiler.ghc8107;
  ghc8107Splices = pkgs.haskell-nix.compiler.ghc8107Splices;
}
