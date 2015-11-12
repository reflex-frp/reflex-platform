{ nixpkgsFunc ? import ./nixpkgs
, system ? null
, config ? null
}:
let nixpkgs = nixpkgsFunc ({
      config = {
        allowUnfree = true;
      } // (if config == null then {} else config);
    } // (
      if system == null then {} else { inherit system; }
    ));
    lib = import "${nixpkgs.path}/pkgs/development/haskell-modules/lib.nix" { pkgs = nixpkgs; };
in with lib;
let overrideCabal = pkg: f: if pkg == null then null else lib.overrideCabal pkg f;
    hspecGit = nixpkgs.fetchgit {
      url = git://github.com/ryantrinkle/hspec;
      rev = "937c0ae61d70dcd71c35a170b800c30f14a5bc9c";
      sha256 = "1819d5b3f973b432339256ba783b33ada691a785d059e83009e5e2edc6178f6d";
    };
    extendHaskellPackages = haskellPackages: haskellPackages.override {
      overrides = self: super: {
        ########################################################################
        # Reflex packages
        ########################################################################
        reflex = self.callPackage ./reflex {};
        reflex-dom = self.callPackage ./reflex-dom {};
        reflex-todomvc = self.callPackage ./reflex-todomvc {};

        ########################################################################
        # ghcjs-boot packages
        ########################################################################
        aeson = overrideCabal super.aeson (drv: {
          version = "0.9.0.1";
          sha256 = "1g7qdq7zpyvqwmh4sfhizqpb51cg24lrcj9vq5msz8k896y7vfcj";
        });
        async = overrideCabal super.async (drv: {
          version = "2.0.1.6";
          sha256 = "06fzkqjliccxqiygms7v1xff3wlkg54n9xwzv7m1yxylkzlikjkz";
          jailbreak = true;
        });
        attoparsec = overrideCabal super.attoparsec (drv: {
          version = "0.13.0.0";
          sha256 = "12b4xi6nlnhpwz8apn4mk880mkhcv1sfvf4j3z1h5dgkadi2zgbi";
        });
        case-insensitive = overrideCabal super.case-insensitive (drv: {
          version = "1.2.0.4";
          sha256 = "07nm40r9yw2p9qsfp3pjbsmyn4dabrxw34p48171zmccdd5hv0v3";
        });
        dlist = overrideCabal super.dlist (drv: {
          version = "0.7.1.1";
          sha256 = "1zayvxvkan2s2ixajdr3f5rn1gzhprzv6cww4cbpwjhzw0l7zc08";
        });
        extensible-exceptions = overrideCabal super.extensible-exceptions (drv: {
          version = "0.1.1.3";
          sha256 = "1i8rjfczsx1wjfaq423a7cp7qrnxh053865z7bg6hwhk2pxsrxkm";
        });
        hashable = overrideCabal super.hashable (drv: {
          version = "1.2.3.2";
          sha256 = "0h9295pv2sgbaqlwpwbx2bap6nngm0jcdhkqham1wpjwyxqgqrlc";
        });
        mtl = overrideCabal super.mtl (drv: {
          version = "2.2.1";
          sha256 = "1icdbj2rshzn0m1zz5wa7v3xvkf6qw811p4s7jgqwvx1ydwrvrfa";
        });
        old-locale = overrideCabal super.old-locale (drv: {
          version = "1.0.0.7";
          sha256 = "0l3viphiszvz5wqzg7a45zp40grwlab941q5ay29iyw8p3v8pbyv";
        });
        old-time = overrideCabal super.old-time (drv: {
          version = "1.1.0.3";
          sha256 = "1h9b26s3kfh2k0ih4383w90ibji6n0iwamxp6rfp2lbq1y5ibjqw";
        });
        parallel = overrideCabal super.parallel (drv: {
          version = "3.2.0.6";
          sha256 = "0hp6vf4zxsw6vz6lj505xihmnfhgjp39c9q7nyzlgcmps3xx6a5r";
        });
        primitive = overrideCabal super.primitive (drv: {
          version = "0.5.4.0";
          sha256 = "05gdgj383xdrdkhxh26imlvs8ji0z28ny38ms9snpvv5i8l2lg10";
          revision = "1";
          editedCabalFile = "df0a129c168c61a06a02123898de081b1d0b254cce6d7ab24b8f43ec37baef9e";
          });
        scientific = overrideCabal super.scientific (drv: {
          version = "0.3.3.3";
          sha256 = "1hngkmd1kggc84sz4mddc0yj2vyzc87dz5dkkywjgxczys51mhqn";
          jailbreak = true;
        });
        stm = overrideCabal super.stm (drv: {
          version = "2.4.4";
          sha256 = "0gc8zvdijp3rwmidkpxv76b4i0dc8dw6nbd92rxl4vxl0655iysx";
        });
        syb = overrideCabal super.syb (drv: {
          version = "0.5.1";
          sha256 = "0iiqz5mamk1nsij99rypms7dhx5flm2n02k1x6miqgnhg075zc41";
        });
        unordered-containers = overrideCabal super.unordered-containers (drv: {
          version = "0.2.5.1";
          sha256 = "06l1xv7vhpxly75saxdrbc6p2zlgz1az278arfkz4rgawfnphn3f";
        });
        vector = overrideCabal super.vector (drv: {
          version = "0.10.12.2";
          sha256 = "01hc71k1z9m0g0dv4zsvq5d2dvbgyc5p01hryw5c53792yi2fm25";
          jailbreak = true;
        });

        ########################################################################
        # Fixups for older ghcjs
        ########################################################################
        webkitgtk3 = overrideCabal super.webkitgtk3 (drv: {
          version = "0.13.1.3";
          sha256 = "0gfznb6n46576im72m6k9wrwc2n9f48nk4dsaz2llvzlzlzx4zfk";
        });
        gtk3 = overrideCabal super.gtk3 (drv: {
          version = "0.13.9";
          sha256 = "1zmcvp295sknc2h529nprclw11lnwp79dniyyg573wc99bdzijvr";
        });
        ghcjs-dom = overrideCabal super.ghcjs-dom (drv: {
          version = "0.1.1.3";
          sha256 = "0pdxb2s7fflrh8sbqakv0qi13jkn3d0yc32xhg2944yfjg5fvlly";
        });

        ########################################################################
        # Fixups for new nixpkgs
        ########################################################################
        language-nix = dontCheck super.language-nix;
        distribution-nixpkgs = dontCheck super.distribution-nixpkgs;

        ########################################################################
        # Other packages
        ########################################################################
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
        ChasingBottoms = overrideCabal super.ChasingBottoms (drv: {
          version = "1.3.0.13";
          sha256 = "1fb86jd6cdz4rx3fj3r9n8d60kx824ywwy7dw4qnrdran46ja3pl";
        });
        doctest = overrideCabal super.doctest (drv: {
          version = "0.9.13";
          revision = "1";
          sha256 = "0xl570ay5bw1rpd1aw59c092rnwjbp9qykh2rhpxyvl333p8mg00";
          editedCabalFile = "592ab6d62eca8a0b43930f15c8fb653c54d60983bd232ecc505bd5a5aebe6f7f";
        });
        haskell-src-meta = overrideCabal super.haskell-src-meta (drv: {
          version = "0.6.0.10";
          sha256 = "0flcyimibz4flq66isshn2zsmzlly6sja6gfb0a0xn4ns4xpwpy1";
        });
        haddock = overrideCabal super.haddock (drv: {
          version = "2.16.1";
          sha256 = "1mnnvc5jqp6n6rj7xw8wdm0z2xp9fndkz11c8p3vbljsrcqd3v26";
          doCheck = false;
        });
        haddock-api = overrideCabal super.haddock-api (drv: {
          version = "2.16.1";
          sha256 = "1spd5axg1pdjv4dkdb5gcwjsc8gg37qi4mr2k2db6ayywdkis1p2";
          doCheck = false;
        });
        haddock-library = overrideCabal super.haddock-library (drv: {
          version = "1.2.1";
          sha256 = "0mhh2ppfhrvvi9485ipwbkv2fbgj35jvz3la02y3jlvg5ffs1c8g";
          doCheck = false;
        });
        QuickCheck = overrideCabal super.QuickCheck (drv: {
          version = "2.8.1";
          sha256 = "0fvnfl30fxmj5q920l13641ar896d53z0z6z66m7c1366lvalwvh";
        });
        exceptions = overrideCabal super.exceptions (drv: {
          jailbreak = true;
        });
        cassava = overrideCabal super.cassava (drv: {
          jailbreak = true;
        });
        gps = dontCheck super.gps; # The test suite for gps pulls in gpx-conduit, which doesn't build
      };
    };
in rec {
  inherit nixpkgs overrideCabal extendHaskellPackages;
  ghc = extendHaskellPackages nixpkgs.pkgs.haskell.packages.ghc7102;
  ghcjsCompiler = overrideCabal (ghc.callPackage "${nixpkgs.path}/pkgs/development/compilers/ghcjs" {
    ghc = nixpkgs.pkgs.haskell.compiler.ghc7102;
    ghcjsBoot = nixpkgs.fetchgit {
      url = git://github.com/ghcjs/ghcjs-boot.git;
      rev = "d435c60b62d24b7a4117493f7aaecbfa09968fe6"; # 7.10 branch
      sha256 = "4159b20730822ec699b47036791158bc32d5412903005d19c396b120beac701f";
      fetchSubmodules = true;
    };
    shims = nixpkgs.fetchgit {
      url = git://github.com/ghcjs/shims.git;
      rev = "0b670ca27fff3f0bad515c37e56ccb8b4d6758fb"; # master branch
      sha256 = "08c0c3547e06d7716b2feb6ebda02f2ab33c205700848ad8e134152f5c3af8a7";
    };
  }) (drv: {
    src = nixpkgs.fetchgit {
      url = git://github.com/ghcjs/ghcjs.git;
      rev = "fb1faa9cb0a11a8b27b0033dfdb07aafb6add35e"; # master branch
      sha256 = "9069f484da55bf5af8dc65e539f86ca5e1b64ab9ecef65f38006c954400a0eef";
    };
  });
  ghcjsPackages = nixpkgs.callPackage "${nixpkgs.path}/pkgs/development/haskell-modules" {
    ghc = ghcjsCompiler;
    packageSetConfig = nixpkgs.callPackage "${nixpkgs.path}/pkgs/development/haskell-modules/configuration-ghcjs.nix" { };
  };

  ghcjs = extendHaskellPackages ghcjsPackages;
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

  workOn = package: (overrideCabal package (drv: {
    buildDepends = drv.buildDepends ++ [ ghc.cabal-install ghc.ghcid ];
  })).env;

  # The systems that we want to build for on the current system
  cacheTargetSystems =
    if nixpkgs.stdenv.system == "x86_64-linux"
    then [ "x86_64-linux" "i686-linux" ] # On linux, we want to build both 32-bit and 64-bit versions
    else [ nixpkgs.stdenv.system ];

  inherit lib;
}
