# This is important to have for jsaddle, reflex, reflex-dom-core, and
# reflex-dom, at the very least; I suspect it's important for a lot
# more stuff, as well.

{ }:

self: super: {
  mkDerivation = drv: super.mkDerivation (drv // {
    configureFlags = (drv.configureFlags or []) ++ [
      "--${if self.ghc.isGhcjs or false then "ghcjs" else "ghc"}-options=-fexpose-all-unfoldings"
    ];
  });
}
