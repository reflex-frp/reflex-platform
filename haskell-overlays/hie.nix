{ haskellLib
, fetchFromGitHub
, dep
}:
let
  ghc-mod-src = fetchFromGitHub {
    owner = "alanz";
    repo = "ghc-mod";
    rev = "b20536757f34769c6fe4478f13b71a55c9ae582e";
    sha256 = "1m5q8znw2lqvhg1sl1cjrw9ywxrnbbrmpb0vc5x8daxp0i7d74gr";
  };
in self: super: {
  haskell-lsp = self.callHackage "haskell-lsp" "0.8.0.1" {};
  haskell-lsp-types = self.callHackage "haskell-lsp-types" "0.8.0.1" {};
  # https://github.com/bubba/lsp-test/commit/a9cff941ac28ce31c8463b6bd84237c0eeee06ea
  # lsp-test = self.callHackage "lsp-test" "0.5.0.2" {};
  lsp-test = haskellLib.dontCheck (self.callCabal2nix "lsp-test" (fetchFromGitHub {
    owner = "bubba";
    repo = "lsp-test";
    rev = "a9cff941ac28ce31c8463b6bd84237c0eeee06ea";
    sha256 = "1607mbw8q10mjy1yy7jzllpa5a4mzj9iy78br5sypnz5lqk780wg";
  }) {});
  hedgehog = haskellLib.doJailbreak super.hedgehog;
  pretty-show = self.callHackage "pretty-show" "1.8.1" {};
  constrained-dynamic = haskellLib.doJailbreak super.constrained-dynamic;
  cabal-helper = haskellLib.doJailbreak (self.callCabal2nix "cabal-helper" (fetchFromGitHub {
    owner = "alanz";
    repo = "cabal-helper";
    rev = "8fecf6a7754424ed1653dce632382707e8f03499";
    sha256 = "1lypls1zkyg7pq2xcg6mm25x7rgndy2553yakdkayg4m8msr7f8f";
  }) {});
  monad-dijkstra = self.callCabal2nix "monad-dijkstra" (fetchFromGitHub {
    owner = "ennocramer";
    repo = "monad-dijkstra";
    rev = "53a1bf93774171fe722c6470e8a03ac0cb0f22fa"; # 0.1.1.2
    sha256 = "08fzq8m81m831bg0w86cqwpskmf892np7wjvpk90m9jd5cdg4rd5";
  }) {};
  floskell = self.callCabal2nix "floskell" (fetchFromGitHub {
    owner = "ennocramer";
    repo = "floskell";
    rev = "059abe627647d97c23579a14194d143d84e3e2f1";
    sha256 = "1nzdb1kf6q1kynln4iqcm5mkswh4a0cl1a91inycbaiq82y9r739";
  }) {};
  HaRe = haskellLib.dontHaddock (haskellLib.dontCheck (haskellLib.doJailbreak (self.callCabal2nix "HaRe" (fetchFromGitHub {
    owner = "alanz";
    repo = "HaRe";
    rev = "53979f062bebcaa132390d1fd0cec74a51662952";
    sha256 = "1986pb1h5fahas38igj71yz4r6x31hml4v1a59gb63q2xivp6sip";
  }) {})));
  ghc-mod = haskellLib.dontCheck (haskellLib.doJailbreak (self.callCabal2nix "ghc-mod" ghc-mod-src {}));
  ghc-mod-core = haskellLib.doJailbreak (self.callCabal2nix "ghc-mod-core" (ghc-mod-src + "/core") {});
  hie-plugin-api = self.callCabal2nix "hie-ide-engine" (dep.haskell-ide-engine + "/hie-plugin-api") {};
  haskell-ide-engine = haskellLib.dontCheck (self.callCabal2nix "haskell-ide-engine" dep.haskell-ide-engine {});
}
