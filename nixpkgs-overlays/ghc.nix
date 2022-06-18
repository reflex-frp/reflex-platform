{ lib, pkgs }:
self: super: {
  # Apply custom patches to Haskell compilers
  haskell = super.haskell // {
    compiler = super.haskell.compiler // {
      ghc8107 = super.haskell.compiler.ghc8107.overrideAttrs (drv:
        let bootPkgs = drv.passthru.bootPkgs; in {
              nativeBuildInputs = with pkgs; [
                perl autoconf269 automake m4 python3 sphinx
                bootPkgs.ghc
                bootPkgs.alex bootPkgs.happy_1_19_12 bootPkgs.hscolour
              ];
            });
    };
  };
}
