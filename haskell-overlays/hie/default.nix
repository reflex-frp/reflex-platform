{ haskellLib
, fetchFromGitHub
, nixpkgs
, thunkSet
}:
with haskellLib;
let
  localDeps = thunkSet ./dep;
  hieSrc = localDeps.haskell-ide-engine;
  ghcModSrc = hieSrc + "/submodules/ghc-mod";
  ghcProjectTypesSrc = ghcModSrc + "/ghc-project-types";
  ghcModCoreSrc  = ghcModSrc + "/core";
  HaReSrc = hieSrc + "/submodules/HaRe";
  cabalHelperSrc = hieSrc + "/submodules/cabal-helper";
in self: super: {
  _dep = super._dep or {} // localDeps;
  constrained-dynamic = doJailbreak super.constrained-dynamic;
  monad-dijkstra = dontCheck (self.callHackage "monad-dijkstra" "0.1.1.2" {}); # This package fails its own hlint test
  floskell = doJailbreak (dontCheck (self.callHackage "floskell" "0.10.1" {}));
  ghc-exactprint = if nixpkgs.stdenv.isDarwin then dontCheck super.ghc-exactprint else super.ghc-exactprint;
  cabal-helper = doJailbreak (self.callCabal2nix "cabal-helper" cabalHelperSrc {});
  ghc-mod = dontCheck (doJailbreak (self.callCabal2nix "ghc-mod" ghcModSrc {}));
  ghc-mod-core = doJailbreak (self.callCabal2nix "ghc-mod-core" ghcModCoreSrc {});
  ghc-project-types = doJailbreak (self.callCabal2nix "ghc-project-types" ghcProjectTypesSrc {});
  hie-plugin-api = self.callCabal2nix "hie-ide-engine" (hieSrc + "/hie-plugin-api") {};
  haskell-ide-engine = dontHaddock (dontCheck (self.callCabal2nix "haskell-ide-engine" hieSrc {}));
  HaRe = dontHaddock (dontCheck (doJailbreak (self.callCabal2nix "HaRe" HaReSrc {})));
  haskell-lsp = self.callHackage "haskell-lsp" "0.15.0.0" {};
  haskell-lsp-types = self.callHackage "haskell-lsp-types" "0.15.0.0" {};
  rope-utf16-splay = self.callHackage "rope-utf16-splay" "0.3.1.0" {};
  unix-time = self.callHackage "unix-time" "0.4.7" {};
  fold-debounce = (if nixpkgs.hostPlatform.isDarwin then dontCheck else (x: x)) super.fold-debounce;
}
