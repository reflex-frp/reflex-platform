{ lib } :
lib.warn "don't import reflex-platform/nixpkgs-overlays/hack-get,  import reflex-platform/nixpkgs-overlays/thunk-import instead"
(self :
  import ../thunk-import { inherit lib; hideDeprecated = false; } self
)