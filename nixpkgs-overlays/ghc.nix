{ lib }:

self: super: {
  cabal2nix-unwrapped = super.haskell.packages.ghc8107.cabal2nix;
  # Apply custom patches to Haskell compilers
  haskell = super.haskell // {
  };
}
