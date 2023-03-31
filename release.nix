{ example ? import ./example/default.nix { } }: {
  ghcjs-app = example.ghcjs-app;
}
