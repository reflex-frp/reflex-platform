{ haskellLib, nixpkgs, thunkSet }:

self: super:

{
  _dep = super._dep // thunkSet ./dep;

  ghc = (import "${nixpkgs.path}/pkgs/development/compilers/ghcjs/8.0" {
    bootPkgs = nixpkgs.haskell.packages.ghc802.override {
      overrides = self: super: {
        # Newer versions no longer export `(<>)`, because that is now
        # understand to be monoid/semigroup append.
        wl-pprint-text = haskellLib.doJailbreak (self.callHackage "wl-pprint-text" "1.1.1.0" {});
        # Old `wl-pprint-text` in turn doesn't expect `base-compat` to provide
        # a `(<>)`, since it is defining its own.
        base-compat = self.callHackage "base-compat" "0.9.3" {};
        # relax bounds for newer process
        concurrent-output = haskellLib.doJailbreak super.concurrent-output;
        # missing semigroups pkg
        ListLike = haskellLib.addBuildDepend super.ListLike self.semigroups;
      };
    };
    inherit (nixpkgs) cabal-install;
    inherit (nixpkgs.buildPackages) fetchgit fetchFromGitHub;
  }).override {
    ghcjsSrc = self._dep."ghcjs";
    ghcjsBootSrc = self._dep."ghcjs-boot";
    shims = self._dep."ghcjs-shims";
    stage2 = import self._self._dep.stage2Script;
  };

  hashable = haskellLib.addBuildDepend (self.callHackage "hashable" "1.2.7.0" {}) self.text;
  # `configure` cannot be generated on the fly from `configure.ac` with older Cabal.
  old-time = haskellLib.addBuildTool super.old-time nixpkgs.autoreconfHook;
}
