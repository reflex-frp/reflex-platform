{ deps, composeExtensions, useTextJSString ? false, useFastWeak ? false }: rec {
  overlays = {
    compilers = import ./compilers.nix { inherit useFastWeak useTextJSString; };
    haskell-nix = import ./haskell.nix;
    android = import ./android.nix;
    add-deps = (final: prev: {
      _dep = deps;
      nix-thunk = final._dep.imported.nix-thunk;
    });
  };

  ordered = deps.imported.haskell-nix.nixpkgsArgs.overlays ++ (with overlays; [
    compilers
    haskell-nix
    add-deps
    android
  ]);

  combined = builtins.foldl' composeExtensions (_: _: { }) ordered;
}
