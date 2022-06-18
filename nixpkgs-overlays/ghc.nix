{ lib }:

self: super: {
  # Apply custom patches to Haskell compilers
  haskell = super.haskell // {
 };
}
