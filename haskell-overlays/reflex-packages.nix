{ haskellLib
, nixpkgs
, fetchFromGitHub, hackGet
, useFastWeak, useReflexOptimizer, enableTraceReflexEvents
}:

with haskellLib;

self: super:

let
  reflexDom = import (hackGet ../reflex-dom) self nixpkgs;
  jsaddlePkgs = import (hackGet ../jsaddle) self;
  gargoylePkgs = self.callPackage (hackGet ../gargoyle) self;
  ghcjsDom = import (hackGet ../ghcjs-dom) self;
  addReflexTraceEventsFlag = drv: if enableTraceReflexEvents
    then appendConfigureFlag drv "-fdebug-trace-events"
    else drv;
  addReflexOptimizerFlag = drv: if useReflexOptimizer && (self.ghc.cross or null) == null
    then appendConfigureFlag drv "-fuse-reflex-optimizer"
    else drv;
  addFastWeakFlag = drv: if useFastWeak
    then enableCabalFlag drv "fast-weak"
    else drv;
in
{
  ##
  ## Reflex family
  ##

  reflex = addFastWeakFlag (addReflexTraceEventsFlag (addReflexOptimizerFlag (self.callPackage (hackGet ../reflex) {})));
  reflex-dom = addReflexOptimizerFlag (doJailbreak reflexDom.reflex-dom);
  reflex-dom-core = addReflexOptimizerFlag (doJailbreak reflexDom.reflex-dom-core);
  reflex-todomvc = self.callPackage (hackGet ../reflex-todomvc) {};
  reflex-aeson-orphans = self.callCabal2nix "reflex-aeson-orphans" (hackGet ../reflex-aeson-orphans) {};

  ##
  ## GHCJS and JSaddle
  ##

  inherit (jsaddlePkgs) jsaddle jsaddle-clib jsaddle-wkwebview jsaddle-webkit2gtk jsaddle-webkitgtk;
  jsaddle-warp = dontCheck jsaddlePkgs.jsaddle-warp;

  jsaddle-dom = overrideCabal (self.callPackage (hackGet ../jsaddle-dom) {}) (drv: {
    # On macOS, the jsaddle-dom build will run out of file handles the first time it runs
    preBuild = ''./setup build || true'';
  });

  inherit (ghcjsDom) ghcjs-dom-jsffi;

  # TODO: Fix this in Cabal
  # When building a package with no haskell files, cabal haddock shouldn't fail
  ghcjs-dom-jsaddle = dontHaddock ghcjsDom.ghcjs-dom-jsaddle;
  ghcjs-dom = dontHaddock ghcjsDom.ghcjs-dom;

  ##
  ## Gargoyle
  ##

  inherit (gargoylePkgs) gargoyle gargoyle-postgresql;

  ##
  ## Misc other dependencies
  ##

  monoidal-containers = self.callCabal2nix "monoidal-containers" (fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "monoidal-containers";
    rev = "79c25ac6bb469bfa92f8fd226684617b6753e955";
    sha256 = "0j2mwf5zhz7cmn01x9v51w8vpx16hrl9x9rcx8fggf21slva8lf8";
  }) {};
}
