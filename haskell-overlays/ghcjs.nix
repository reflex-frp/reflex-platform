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
    rev = "1f28a93dad380a471a3fd7e00819a88c20fa7f92";
    sha256 = "1l1mjpb5a0ryamqapm0lirxp58f7w3hk73b18ks62bc9d342zmxm";
  }) {});

  ghc = super.ghc // {
    withPackages = self.ghcWithPackages;
  };

  diagrams-lib = haskellLib.dontCheck super.diagrams-lib;

}
