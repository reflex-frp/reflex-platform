{ system ? null, config ? null }:
let overrideCabal = drv: f: if drv == null then null else (drv.override (args: args // {
      mkDerivation = drv: args.mkDerivation (drv // f drv);
    })) // {
      overrideScope = scope: overrideCabal (drv.overrideScope scope) f;
    };
    nixpkgs = import ./nixpkgs ({
      config.allowUnfree = true;
    } // (
      if system == null then {} else { inherit system; }
    ) // (
      if config == null then {} else { inherit config; }
    ));
    hspecGit = nixpkgs.fetchgit {
      url = git://github.com/ryantrinkle/hspec;
      rev = "937c0ae61d70dcd71c35a170b800c30f14a5bc9c";
      sha256 = "1819d5b3f973b432339256ba783b33ada691a785d059e83009e5e2edc6178f6d";
    };
    combineOverrides = old: new: (old // new) // {
      overrides = self: super:
        let oldOverrides = old.overrides self super;
        in oldOverrides // new.overrides self (super // oldOverrides);
    };
    makeRecursivelyOverridable = x: old: x.override old // {
      override = new: makeRecursivelyOverridable x (combineOverrides old new);
    };
    extendHaskellPackages = haskellPackages: makeRecursivelyOverridable haskellPackages {
      overrides = self: super: {
        reflex = self.callPackage ./reflex {};
        reflex-dom = self.callPackage ./reflex-dom {};
        reflex-todomvc = self.callPackage ./reflex-todomvc {};
        hspec = overrideCabal super.hspec (drv: {
          version = "2.1.8";
          src = hspecGit;
        });
        hspec-core = overrideCabal super.hspec-core (drv: {
          version = "2.1.9";
          src = hspecGit + "/hspec-core";
          preConfigure = ''
            rm LICENSE
            touch LICENSE
          '';
        });
        hspec-discover = overrideCabal super.hspec-discover (drv: {
          version = "2.1.9";
          src = hspecGit + "/hspec-discover";
          preConfigure = ''
            rm LICENSE
            touch LICENSE
          '';
        });
        hspec-expectations = overrideCabal super.hspec-expectations (drv: {
          version = "0.7.0";
          sha256 = "1gzjnmhi6ia2p5i5jlnj4586rkml5af8f7ijgipzs6fczpx7ds4l";
        });
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
          sha256 = "103jxzzw3drg7pkgmh39s7258zcwr8ixg8mijm6p33b87a8wdpwr";
        });
      };
    };
in rec {
  inherit nixpkgs overrideCabal;
  ghc = extendHaskellPackages nixpkgs.pkgs.haskell-ng.packages.ghc7101;
  ghcjs = extendHaskellPackages nixpkgs.pkgs.haskell-ng.packages.ghcjs;
  platforms = [ "ghcjs" ] ++ (if !nixpkgs.stdenv.isDarwin then [ "ghc" ] else []);

  attrsToList = s: map (name: { inherit name; value = builtins.getAttr name s; }) (builtins.attrNames s);
  mapSet = f: s: builtins.listToAttrs (map ({name, value}: {
    inherit name;
    value = f value;
  }) (attrsToList s));
  mkSdist = pkg: pkg.override {
    mkDerivation = drv: ghc.mkDerivation (drv // {
      postConfigure = ''
        ./Setup sdist
        mkdir "$out"
        mv dist/*.tar.gz "$out/${drv.pname}-${drv.version}.tar.gz"
        exit 0
      '';
    });
  };
  sdists = mapSet mkSdist ghc;
  mkHackageDocs = pkg: pkg.override {
    mkDerivation = drv: ghc.mkDerivation (drv // {
      postConfigure = ''
        ./Setup haddock --hoogle --hyperlink-source --html --html-location='/package/${drv.pname}-${drv.version}/docs' --contents-location='/package/${drv.pname}-${drv.version}' --haddock-option=--built-in-themes
        cd dist/doc/html
        mv "${drv.pname}" "${drv.pname}-${drv.version}-docs"
        mkdir "$out"
        tar cz --format=ustar -f "$out/${drv.pname}-${drv.version}-docs.tar.gz" "${drv.pname}-${drv.version}-docs"
        exit 0
      '';
    });
  };
  hackageDocs = mapSet mkHackageDocs ghc;
  mkReleaseCandidate = pkg: nixpkgs.stdenv.mkDerivation (rec {
    name = pkg.name + "-rc";
    sdist = mkSdist pkg + "/${pkg.pname}-${pkg.version}.tar.gz";
    docs = mkHackageDocs pkg + "/${pkg.pname}-${pkg.version}-docs.tar.gz";

    builder = builtins.toFile "builder.sh" ''
      source $stdenv/setup

      mkdir "$out"
      echo -n "${pkg.pname}-${pkg.version}" >"$out/pkgname"
      ln -s "$sdist" "$docs" "$out"
    '';

    # 'checked' isn't used, but it is here so that the build will fail if tests fail
    checked = overrideCabal pkg (drv: {
      doCheck = true;
      src = sdist;
    });
  });
  releaseCandidates = mapSet mkReleaseCandidate ghc;

  # The systems that we want to build for on the current system
  cacheTargetSystems =
    if nixpkgs.stdenv.system == "x86_64-linux"
    then [ "x86_64-linux" "i686-linux" ] # On linux, we want to build both 32-bit and 64-bit versions
    else [ nixpkgs.stdenv.system ];
}
