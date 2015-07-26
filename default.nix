{ system ? null }:
let overrideCabal = drv: f: (drv.override (args: args // {
      mkDerivation = drv: args.mkDerivation (drv // f drv);
    })) // {
      overrideScope = scope: overrideCabal (drv.overrideScope scope) f;
    };
    nixpkgs = import ./nixpkgs ({
      config.allowUnfree = true;
    } // (if system == null then {} else { inherit system; }));
    extendHaskellPackages = haskellPackages: haskellPackages.override {
      overrides = self: super: {
        reflex = self.callPackage ./reflex {};
        reflex-dom = self.callPackage ./reflex-dom {};
        reflex-todomvc = self.callPackage ./reflex-todomvc {};

	these = overrideCabal super.these (drv: { 
           version ="0.4.1";
           configureFlags = [ "--constraint='vector==0.10.*'"];
	   sha256 = "135qiyg87cf9rl10zb681mwnrwxdm37h76dnlia8amkkmpkg4wia";
	   editedCabalFile = "195bs2vll614x6l3h1p610v31yq4sy5hqw9nc3wc5b5z01xyr4d0";
	   jailbreak = false;
        });
       

	vector = self.callPackage
	  ({ mkDerivation, base, deepseq, ghc-prim, primitive, QuickCheck
	   , random, template-haskell, test-framework
	   , test-framework-quickcheck2, transformers,stdenv
	   }:
	   mkDerivation {
	     pname = "vector";
	     version = "0.10.12.3";
	     sha256 = "16p8i0gvc9d4n9mxlhlnvrl2s0gmgd7kcsk5czdzz2cd4gh5qxhg";
	     buildDepends = [ base deepseq ghc-prim primitive ];
	     testDepends = [
	       base QuickCheck random template-haskell test-framework
	       test-framework-quickcheck2 transformers
	     ];
	     jailbreak = true;
	     homepage = "https://github.com/haskell/vector";
	     description = "Efficient Arrays";
	     license = stdenv.lib.licenses.bsd3;
	   }) {};


        ghcjs-jquery = self.callPackage ({ mkDerivation, data-default, ghcjs-base, ghcjs-dom, text }:
          mkDerivation {
            pname = "ghcjs-jquery";
            version = "0.1.0.0";
            src = nixpkgs.fetchgit {
              url = git://github.com/ghcjs/ghcjs-jquery;
              rev = "c5eeeafcf81c0d3237b8b9fcb98c4b3633a1297f";
              sha256 = "3b2de54224963ee17857a9737b65d49edc423e06ad7e9c9b85d9f69ca923676a";
            };
            buildDepends = [
              data-default ghcjs-base ghcjs-dom text
            ];
            jailbreak = true;
            license = null;
          }
        ) {};


        active = overrideCabal super.active (drv: {
          version = "0.1.0.19";
          sha256 = "1zzzrjpfwxzf0zbz8vcnpfqi7djvrfxglhkvw1s6yj5gcblg2rcw";
          doCheck = false;
        });
        thyme = overrideCabal super.thyme (drv: {
          doCheck = false;
        });
        orgmode-parse = overrideCabal super.orgmode-parse (with self; drv: {
          version = "0.1.0.4";
          sha256 = "098zl8nyph459zyla0y2mkqiy78zp74yzadrnwa6xv07i5zs125h";
          buildDepends = [
            aeson attoparsec free hashable text thyme unordered-containers
          ];
          testDepends = [
            aeson attoparsec hashable HUnit tasty tasty-hunit text thyme
            unordered-containers
          ];
          doCheck = false;
        });
        twitter-types = overrideCabal super.twitter-types (drv: {
          doCheck = false;
        });
        twitter-types-lens = overrideCabal super.twitter-types-lens (drv: {
          doCheck = false;
        });
        HaskellForMaths = overrideCabal super.HaskellForMaths (drv: {
          version = "0.4.8";
          sha256 = "0yn2nj6irmj24j1djvnnq26i2lbf9g9x1wdhmcrk519glcn5k64j";
          buildDepends = [ self.semigroups ] ++ drv.buildDepends; # For some reason, without the spurious import of self.semigroups, HaskellForMaths will fail to build the environment for HaskellForMaths on ghcjs (it works on ghc)
        });
        dependent-sum-template = overrideCabal super.dependent-sum-template (drv: {
          version = "0.0.0.4";
          src = nixpkgs.fetchgit {
            url = git://github.com/ryantrinkle/dependent-sum-template;
            rev = "abcd0f01a3e264e5bc1f3b00f3d03082f091ec49";
            sha256 = "16f95348c559394a39848394a9e1aa8318c79bfc62bc6946edad9aabd20a8e2d";
          };
        });
      };
    };
in rec {
  inherit nixpkgs overrideCabal;
  ghc = extendHaskellPackages nixpkgs.pkgs.haskell-ng.packages.ghc7101;
  ghcjs = extendHaskellPackages nixpkgs.pkgs.haskell-ng.packages.ghcjs;
  platforms = [ "ghcjs" ] ++ (if !nixpkgs.stdenv.isDarwin then [ "ghc" ] else []);

  # The systems that we want to build for on the current system
  cacheTargetSystems =
    if nixpkgs.stdenv.system == "x86_64-linux"
    then [ "x86_64-linux" "i686-linux" ] # On linux, we want to build both 32-bit and 64-bit versions
    else [ nixpkgs.stdenv.system ];
}
