{ haskellLib, nixpkgs, fetchFromGitHub, useReflexOptimizer }:

self: super: {
  ghcWithPackages = selectFrom: self.callPackage (nixpkgs.path + "/pkgs/development/haskell-modules/with-packages-wrapper.nix") {
    inherit (self) llvmPackages;
    haskellPackages = self;
    packages = selectFrom self;
    ${if useReflexOptimizer then "ghcLibdir" else null} = "${self.ghc.bootPackages.ghcWithPackages (p: [ p.reflex ])}/lib/${self.ghc.bootPackages.ghc.name}";
  };

  ghcjs-base = haskellLib.doJailbreak (self.callCabal2nix "ghcjs-base" (fetchFromGitHub {
    owner = "ghcjs";
    repo = "ghcjs-base";
    rev = "43804668a887903d27caada85693b16673283c57";
    sha256 = "1pqmgkan6xhpvsb64rh2zaxymxk4jg9c3hdxdb2cnn6jpx7jsl44";
  }) {});

  ghc = super.ghc // {
    withPackages = self.ghcWithPackages;
  };

  diagrams-lib = haskellLib.dontCheck super.diagrams-lib;
  linear = haskellLib.dontCheck super.linear;
  bytes = haskellLib.dontCheck super.bytes;

  # doctest doesn't work on ghcjs, but sometimes dontCheck doesn't seem to get rid of the dependency
  doctest = builtins.trace "Warning: ignoring dependency on doctest" null;

}
