{ mkDerivation, pkgs, dependent-map, ghcjs-dom, lens
, mtl, ref-tf, reflex, text, these
, transformers, data-default, semigroups, aeson
, ghc, webkitgtk3-javascriptcore, exception-transformers
, webkitgtk24x, dependent-sum-template, bifunctors, bimap
, raw-strings-qq, zenc, random, monad-control
, file-embed #TODO: Get rid of this
}:

mkDerivation {
  pname = "reflex-dom";
  version = "0.3";
  src = builtins.filterSource (path: type: baseNameOf path != ".git") ./.;
  buildDepends = [
    reflex
    dependent-map
    mtl
    transformers
    these
    lens
    ghcjs-dom
    text
    data-default
    semigroups
    ref-tf
    aeson
    exception-transformers
    dependent-sum-template
    bifunctors
    zenc
    random
    bimap
    monad-control
    file-embed #TODO: Get rid of this
  ] ++ (if (ghc.pname or null) == "ghcjs" then [ ] else [ webkitgtk3-javascriptcore raw-strings-qq ]);
  pkgconfigDepends = if (ghc.pname or null) == "ghcjs" then [ ] else [ webkitgtk24x webkitgtk3-javascriptcore raw-strings-qq ];
  license = null;
}
