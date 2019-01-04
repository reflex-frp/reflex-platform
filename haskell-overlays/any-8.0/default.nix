{ haskellLib, nixpkgs, thunkSet, runCommand }:

self: super: {
  _dep = super._dep or {} // thunkSet ./dep // {
    stage2Script = runCommand "stage2.nix" {
      GEN_STAGE2 = builtins.readFile (nixpkgs.path + "/pkgs/development/compilers/ghcjs/gen-stage2.rb");
      buildCommand = ''
        echo "$GEN_STAGE2" > gen-stage2.rb && chmod +x gen-stage2.rb
        patchShebangs .
        ./gen-stage2.rb "${self._dep."ghcjs-boot"}" >"$out"
      '';
      nativeBuildInputs = with nixpkgs; [
        ruby cabal2nix
      ];
    } "";
  };

  # Setup: Unknown build tool hspec-discover
  megaparsec = haskellLib.dontCheck super.megaparsec;
  # Setup: Unknown build tool hspec-discover
  modern-uri = haskellLib.dontCheck super.modern-uri;
  # Version compatible with ghc-mod 0.5.8.0
  cabal-helper = haskellLib.doJailbreak (self.callHackage "cabal-helper" "0.7.3.0" {});
  # missing semigroups pkg
  ListLike = haskellLib.addBuildDepend super.ListLike self.semigroups;
  #
  ghc-mod = haskellLib.doJailbreak super.ghc-mod;
}
