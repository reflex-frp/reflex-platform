{
  example_darwin ? import ./example/default.nix {
    system = "x86_64-darwin";
  },
  example_linux ? import ./example/default.nix {
    system = "x86_64-linux";
  },
  tests_linux ? import ./tests/thunk_tests.nix {
    system = "x86_64-linux";
  },
  tests_macos ? import ./tests/thunk_tests.nix {
    system = "x86_64-darwin";
  },
  default ? import ./default.nix { }
}: {
  ghcjs-app = example_linux.ghcjs-app;
  android-app = example_linux.android.app.aarch64;
  linux-app = example_linux.hsPkgs.reflex-todomvc.components.exes.reflex-todomvc;
  linux-dev = example_linux.shells.ghc;

  ghcjs_macos = example_darwin.ghcjs-app;
  ios-app = example_darwin.ios.app.aarch64;
  darwin-app = example_darwin.hsPkgs.reflex-todomvc.components.exes.reflex-todomvc;
  darwin-dev = example_linux.shells.ghc;


  ghc8107 = default.pkgs.haskell-nix.compiler.ghc8107;
  ghc8107Splices = default.pkgs.haskell-nix.compiler.ghc8107Splices;
  #ghc8107JSSTringGHC = default.pkgs.haskell-nix.compiler.ghcjs8107JSString;

  inherit tests_linux tests_macos;
}
