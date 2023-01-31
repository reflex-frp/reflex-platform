{ nix-thunk, useTextJSString ? false, useFastWeak ? false }: let
  composeExtensions =
    f: g: final: prev:
      let fApplied = f final prev;
          prev' = prev // fApplied;
      in fApplied // g final prev';
in rec {
  overlays = {
    thunk = (final: prev: {
      inherit nix-thunk;
    });
    compilers = import ./compilers.nix { inherit useFastWeak useTextJSString; };
    haskell-nix = import ./haskell.nix;
    android = import ./android.nix;
  };

  ordered = with overlays; [
    compilers
    haskell-nix
    thunk
    android
  ];

  combined = builtins.foldl' composeExtensions (_: _: { }) ordered;
}
