{ lib, pkgs }:
self: super: {
  # Apply custom patches to Haskell compilers
  haskell = super.haskell // {
    compiler = super.haskell.compiler // {
      ghc8107 = super.haskell.compiler.ghc8107.overrideAttrs (drv:
        let bootPkgs = drv.passthru.bootPkgs; in {
              nativeBuildInputs = with pkgs; [
                perl autoconf automake m4
                bootPkgs.ghc
                bootPkgs.alex bootPkgs.happy_1_19_12 bootPkgs.hscolour
              ];
              enableDocs = false;
            });
    };
  };
}
