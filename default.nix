{ nixpkgsFunc ? import ./nixpkgs
, system ? null
, config ? null
, enableLibraryProfiling ? false
}:
let nixpkgs = nixpkgsFunc ({
      config = {
        allowUnfree = true;
        allowBroken = true; # GHCJS is marked broken in 011c149ed5e5a336c3039f0b9d4303020cff1d86
      } // (if config == null then {} else config);
    } // (
      if system == null then {} else { inherit system; }
    ));
    lib = import "${nixpkgs.path}/pkgs/development/haskell-modules/lib.nix" { pkgs = nixpkgs; };
    gtk2hsSrc = nixpkgs.fetchgit {
      url = "git://github.com/gtk2hs/gtk2hs";
      rev = "eee61d84edf1dd44f8d380d7d7cae2405de50124";
      sha256 = "00j8ssgdbins0kprq1mnfvr18nly50h10da7sw9h4nxdb58z968n";
    };
in with lib;
let overrideCabal = pkg: f: if pkg == null then null else lib.overrideCabal pkg f;
    replaceSrc = pkg: src: version: overrideCabal pkg (drv: {
      inherit src version;
      sha256 = null;
      revision = null;
      editedCabalFile = null;
    });
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
    cabal2nixResult = src: nixpkgs.runCommand "cabal2nixResult" {
      buildCommand = ''
        cabal2nix file://"${src}" >"$out"
      '';
      buildInputs = with nixpkgs; [
        cabal2nix
      ];

      # Support unicode characters in cabal files
      ${if !nixpkgs.stdenv.isDarwin then "LOCALE_ARCHIVE" else null} = "${nixpkgs.glibcLocales}/lib/locale/locale-archive";
      ${if !nixpkgs.stdenv.isDarwin then "LC_ALL" else null} = "en_US.UTF-8";
    } "";
    extendHaskellPackages = haskellPackages: makeRecursivelyOverridable haskellPackages {
      overrides = self: super: {
        ########################################################################
        # Reflex packages
        ########################################################################
        reflex = self.callPackage ./reflex {};
        reflex-dom = self.callPackage ./reflex-dom {};
        reflex-todomvc = self.callPackage ./reflex-todomvc {};

        # GHC 8.0 stuff
        statistics = dontHaddock super.statistics;
        ref-tf = doJailbreak super.ref-tf;
        ed25519 = dontCheck super.ed25519;
        hackage-security = dontHaddock (dontCheck super.hackage-security);
        th-extras = replaceSrc super.th-extras (nixpkgs.fetchgit (builtins.fromJSON ''{
          "url": "git://github.com/ryantrinkle/th-extras",
          "rev": "a20dd7d5ea72a821df09ac834bddeeb7df85056f",
          "sha256": "19zimnsgksq3fz6c33mpx7hyxp1l74pw18g6cq2cxfksr3lgdf03"
        }'')) "0.0.0.4";
        gtk2hs-buildtools = replaceSrc super.gtk2hs-buildtools "${gtk2hsSrc}/tools" "0.13.0.5";
        glib = replaceSrc super.glib "${gtk2hsSrc}/glib" "0.13.2.2";
        gio = replaceSrc super.gio "${gtk2hsSrc}/gio" "0.13.1.1";
        gtk3 = replaceSrc super.gtk3 "${gtk2hsSrc}/gtk" "0.14.2";
        cairo = replaceSrc super.cairo "${gtk2hsSrc}/cairo" "0.13.1.1";
        pango = replaceSrc super.pango "${gtk2hsSrc}/pango" "0.13.1.1";
        dependent-sum = replaceSrc super.dependent-sum (nixpkgs.fetchgit (builtins.fromJSON ''{
          "url": "git://github.com/ryantrinkle/dependent-sum",
          "rev": "db0322e0e0727b40da1c7ac299bb8ef09a7c5b0c",
          "sha256": "1k8bd6a883wr44p970hl2kycwx7ivww98ks58qqflk94j80sjajq"
        }'')) "0.3.2.2";
        dependent-sum-template = self.callPackage (cabal2nixResult (nixpkgs.fetchgit (builtins.fromJSON ''{
          "url": "git://github.com/ryantrinkle/dependent-sum-template",
          "rev": "a6b4f84bc5b5ba186d8828401517feef7707ca20",
          "sha256": "0if567cj28rd3qhiixfxgzjzijxs7hmivgq3zsaxn3jnjr9c557w"
        }''))) {};
        MemoTrie = dontHaddock super.MemoTrie;
        deepseq-generics = doJailbreak super.deepseq-generics;
        ghcjs-dom = replaceSrc super.ghcjs-dom (nixpkgs.fetchgit (builtins.fromJSON ''{
          "url": "git://github.com/ryantrinkle/ghcjs-dom",
          "rev": "8a53f7c2510fd15e61e419dea45bc644c7c05e51",
          "sha256": "1kh53p4d8qnlwn6wfajvd6k9jrv0snjzwwflhrxgyv8nfhb7jr1c"
        }'')) "0.2.3.1";
        webkitgtk3 = self.callPackage (cabal2nixResult (nixpkgs.fetchgit (builtins.fromJSON ''{
          "url": "git://github.com/ryantrinkle/webkit",
          "rev": "be8046844e8108f88407a2e5548d0c19911cb012",
          "sha256": "1hahvjazfh620liicz046196k4gv7fx28vmc9pa3ys4z5szlzdm4"
        }''))) { webkit = nixpkgs.webkitgtk24x; };
        webkitgtk3-javascriptcore = self.callPackage (cabal2nixResult (nixpkgs.fetchgit (builtins.fromJSON ''{
          "url": "git://github.com/ryantrinkle/webkit-javascriptcore",
          "rev": "f3f4a05754ee0d76511ab7aaa6d080b4212d80e8",
          "sha256": "02nfzvkahikkj1ji2idviwj07zzw4cpvysp2341p2zd764kz9p55"
        }''))) { webkit = nixpkgs.webkitgtk24x; };

        timezone-series = replaceSrc super.timezone-series (nixpkgs.fetchgit (builtins.fromJSON ''{
          "url": "git://github.com/ryantrinkle/timezone-series",
          "rev": "f8dece8c016db6476e2bb0d4f972769a76f6ff40",
          "sha256": "1x7qdjmaahs8hg1azki34aq5h971gqnv2hlyb1y8a1s0ff9ri122"
        }'')) "0.1.5.2";
        MonadCatchIO-transformers = doJailbreak super.MonadCatchIO-transformers;
        blaze-builder-enumerator = doJailbreak super.blaze-builder-enumerator;
        diagrams-lib = dontHaddock (appendConfigureFlag super.diagrams-lib "--ghc-option=-XConstrainedClassMethods");
        diagrams-contrib = doJailbreak super.diagrams-contrib;
        diagrams-svg = doJailbreak super.diagrams-svg;
        either = overrideCabal super.either (drv: {
          version = "4.4.1.1";
          sha256 = "1lrlwqqnm6ibfcydlv5qvvssw7bm0c6yypy0rayjzv1znq7wp1xh";
          revision = null;
          editedCabalFile = null;
        });

        ########################################################################
        # Fixups for new nixpkgs
        ########################################################################
        language-nix = dontCheck super.language-nix;
        distribution-nixpkgs = dontCheck super.distribution-nixpkgs;

        ########################################################################
        # ghcjs-boot packages
        ########################################################################
/*
        aeson = overrideCabal super.aeson (drv: {
          version = "0.9.0.1";
          sha256 = "1g7qdq7zpyvqwmh4sfhizqpb51cg24lrcj9vq5msz8k896y7vfcj";
          revision = "1";
          editedCabalFile = "08py84nx8zw5bqm2ns66xkjjhiyp1n3ba7yc6h1csc17rp0jf6v5";
        });
        haskell-src-exts = overrideCabal super.haskell-src-exts (drv: {
          version = "1.16.0.1";
          sha256 = "1h8gjw5g92rvvzadqzpscg73x7ajvs1wlphrh27afim3scdd8frz";
          revision = null;
          editedCabalFile = null;
        });
        hlint = overrideCabal super.hlint (drv: {
          version = "1.9.22";
          sha256 = "1xqrbk6ia488992jj8ms1p2xcs7fpyladh9q7gl8g6w971r3fvjz";
        });
        transformers-compat = overrideCabal super.transformers-compat (drv: {
          version = "0.4.0.4";
          sha256 = "0lmg8ry6bgigb0v2lg0n74lxi8z5m85qq0qi4h1k9llyjb4in8ym";
        });
        bifunctors = overrideCabal super.bifunctors (drv: {
          version = "5.2";
          sha256 = "056y923znv08zxqyabaas91yg56ysbmb2jml0j27nfl6qpd77qa6";
        });
        optparse-applicative = overrideCabal super.optparse-applicative (drv: {
          version = "0.11.0.2";
          sha256 = "0ni52ii9555jngljvzxn1ngicr6i2w647ww3rzhdrmng04y95iii";
        });
        syb = overrideCabal super.syb (drv: {
          version = "0.5.1";
          sha256 = "0iiqz5mamk1nsij99rypms7dhx5flm2n02k1x6miqgnhg075zc41";
        });
        tar = overrideCabal super.tar (drv: {
          version = "0.4.5.0";
          sha256 = "0apk6fz44bs7b6jfqviyi3374gkjy4w8wmds4gq6j6clbsxxfn99";
        });
        # For some reason, without this, ghcjs gets an old version of dependent-sum when building dependent-sum-template
        dependent-sum-template = self.callPackage (
          { mkDerivation, base, dependent-sum, stdenv, template-haskell
          , th-extras
          }:
          mkDerivation {
            pname = "dependent-sum-template";
            version = "0.0.0.4";
            sha256 = "103jxzzw3drg7pkgmh39s7258zcwr8ixg8mijm6p33b87a8wdpwr";
            libraryHaskellDepends = [
              base dependent-sum template-haskell th-extras
            ];
            homepage = "/dev/null";
            description = "Template Haskell code to generate instances of classes in dependent-sum package";
            license = stdenv.lib.licenses.publicDomain;
          }
        ) {};

        # For compatibility with the above
        derive = overrideCabal super.derive (drv: {
          version = "2.5.22";
          sha256 = "0g2grz9y23n8g4wwjinx5cc70aawswl84i3njgj6l1fl29fk1yf2";
        });
*/
        # The lens tests take WAY too long to run
        lens = dontCheck super.lens;

        /*
        these = overrideCabal super.these (drv: { 
          version = "0.5.0.0";
          src = nixpkgs.fetchgit {
            url = git://github.com/ryantrinkle/these;
            rev = "36e7dc3e55c85b2d501c7ddc5e77a9a6bb522db2";
            sha256 = "8841dd7426ad5e0edd05599a0896a6033043f8fa7faf6f7f4c6f88ef1d0209c7";
          };
          revision = null;
          editedCabalFile = null;
        });
        lens = overrideCabal super.lens (drv: {
          version = "4.12.3";
          sha256 = "0898z1ws9sy77yfhvx5did0pibpp81yxz0jg418gdx3znd39vyj8";
        });
        profunctors = overrideCabal super.profunctors (drv: {
          version = "5.1.1";
          sha256 = "0lw2ipacpnp9yqmi8zsp01pzpn5hwj8af3y0f3079mddrmw48gw7";
          revision = null;
          editedCabalFile = null;
        });
        bifunctors = overrideCabal super.bifunctors (drv: {
          version = "5";
          sha256 = "13990xdgx0n23qgi18ghhmsywj5zkr0a5bim0g8a4nzi0cx95ps1";
          buildDepends = with self; [
            semigroups
            tagged
          ];
        });
        reflection = overrideCabal super.reflection (drv: {
          version = "2";
          sha256 = "1hlrji6wyqr892a78px7wilhywyiqdfdmnr7zp4c641qks4rw6gf";
        });
        adjunctions = overrideCabal super.adjunctions (drv: {
          version = "4.2.1";
          sha256 = "0vzlz2q6863ywnhvax3m5xq99x6bz1h47z7z8hmnqdfg5pa4r9k5";
        });
        kan-extensions = overrideCabal super.kan-extensions (drv: {
          version = "4.2.2";
          sha256 = "0dqqlrzrhz8di5hp4kby3205inpj2r30bl75zyy24nq4hgans7g5";
          revision = null;
          editedCabalFile = null;
        });
        free = overrideCabal super.free (drv: {
          version = "4.12.1";
          sha256 = "0sr8phvrb4ny8j1wzq55rdn8q4br23q4pw2j276npr844825jr9p";
          buildDepends = (drv.buildDepends or []) ++ (with self; [
            exceptions
          ]);
        });
        semigroupoids = overrideCabal super.semigroupoids (drv: {
          version = "5.0.0.2";
          sha256 = "14q7284gq44h86j6jxi7pz1hxwfal0jgv6i2j1v2hdzqfnd8z5sw";
          revision = null;
          editedCabalFile = null;
          buildDepends = (drv.buildDepends or []) ++ (with self; [
            base-orphans
            bifunctors
          ]);
        });
        comonad = overrideCabal super.comonad (drv: {
          version = "4.2.7.2";
          sha256 = "0arvbaxgkawzdp38hh53akkahjg2aa3kj2b4ns0ni8a5ylg2cqmp";
        });
        either = overrideCabal super.either (drv: {
          version = "4.4.1";
          sha256 = "1jq9b7mwljyqxmcs09bnqzza6710sfk2x444p3aagjlvq3mpvrci";
          buildDepends = drv.buildDepends ++ (with self; [
            mmorph
          ]);
        });
        monoid-extras = overrideCabal super.monoid-extras (drv: {
          version = "0.4.0.1";
          sha256 = "0jcyjqmk4s64j05qisvibmy87m5xi5n837wsivq7lml8lfyrj7yf";
        });
        linear = overrideCabal super.linear (drv: {
          version = "1.19.1.3";
          sha256 = "1hprmhs1nm6l81kpnnznz92l66j10z4asn3g3l9c47165q881592";
        });
        vector-algorithms = overrideCabal super.vector-algorithms (drv: {
          jailbreak = true;
        });
        vector = overrideCabal super.vector (drv: {
          version = "0.11.0.0";
          sha256 = "1r1jlksy7b0kb0fy00g64isk6nyd9wzzdq31gx5v1wn38knj0lqa";
        });
        ghcjs-dom = overrideCabal super.ghcjs-dom (drv: {
          version = "0.2.1.0";
          src = nixpkgs.fetchgit {
            url = git://github.com/ghcjs/ghcjs-dom;
            rev = "d6eb927ae279071495f5edd7413bef517508bc7d";
            sha256 = "c5cc066fd16a7838b6cb51d151d8d01264ac682228fd1730a9f08cf3437c6f3c";
          };
        });
        webkitgtk3 = overrideCabal super.webkitgtk3 (drv: {
          version = "0.14.1.0";
          src = nixpkgs.fetchgit {
            url = git://github.com/gtk2hs/webkit;
            rev = "482e30764bcfd8207347fd71027d4c8e1f423ee4";
            sha256 = "280eae67462787cc737ddf56178c54a9f6f2c7d308366e2dbe638c331d6e3a1b";
          };
        });
        webkitgtk3-javascriptcore = overrideCabal super.webkitgtk3-javascriptcore (drv: {
          version = "0.13.1.0";
          src = nixpkgs.fetchgit {
            url = git://github.com/gtk2hs/webkit-javascriptcore;
            rev = "555064049fadd0a83a72d315232040efce1fd0bd";
            sha256 = "04f12913d7d4a9818f3fe0c27dd57489a41adf59d8fffdf9eaced084feb34d05";
          };
        });

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
        diagrams-core = overrideCabal super.diagrams-core (drv: {
          jailbreak = true;
        });
        diagrams-lib = overrideCabal super.diagrams-lib (drv: {
          jailbreak = true;
        });
        diagrams-contrib = overrideCabal super.diagrams-contrib (drv: {
          jailbreak = true;
        });
        force-layout = overrideCabal super.force-layout (drv: {
          jailbreak = true;
        });
        active = overrideCabal super.active (drv: {
          version = "0.2.0.4";
          sha256 = "1xm2y8knqhd883c41194h323vchv4hx57wl32l9f64kf7gdglag0";
        });
        snap = overrideCabal super.snap (drv: {
          version = "0.14.0.6";
          sha256 = "05xnil6kfxwrnbvg7sigzh7hl8jsfr8cvbjd41z9ywn6ymxzr7zs";
          revision = null;
          editedCabalFile = null;
        });
        ad = overrideCabal super.ad (drv: {
          version = "4.2.3";
          sha256 = "0w9nd8llzcjb91x1d3mh5482pavbx1jpn8w2ahm6ydjwvijjd9r5";
        });
        */
      } // (if enableLibraryProfiling 
            then {
             mkDerivation = expr: super.mkDerivation (
             expr // { enableLibraryProfiling = true; }
             );
            }
            else {});
    };
    cabalSrc = nixpkgs.fetchgit (builtins.fromJSON ''{
      "url": "git://github.com/haskell/cabal",
      "rev": "1ce7f185c1108eab9b8cebf704751154e8a4dcdc",
      "sha256": "103n9q43rl2n3vga18f60q3wiibnjsf02fnm1hbn4p365yqfyy3v"
    }'');
    overrideForGhc8 = haskellPackages: haskellPackages.override {
      overrides = self: super: {
        Cabal = dontCheck (self.callPackage (cabal2nixResult "${cabalSrc}/Cabal") {});
        cabal-install = dontCheck (self.callPackage (cabal2nixResult "${cabalSrc}/cabal-install") {});
      };
    };
in rec {
  inherit nixpkgs overrideCabal extendHaskellPackages;
  ghc = overrideForGhc8 (extendHaskellPackages nixpkgs.pkgs.haskell.packages.ghc801);
  ghc7 = extendHaskellPackages nixpkgs.pkgs.haskell.packages.ghc7103;
  ghcjsCompiler = overrideCabal (ghc7.callPackage "${nixpkgs.path}/pkgs/development/compilers/ghcjs" {
    bootPkgs = ghc7;
    ghcjsBootSrc = nixpkgs.fetchgit (builtins.fromJSON (builtins.readFile ./ghcjs-boot/git.json));
    shims = nixpkgs.fetchgit (builtins.fromJSON (builtins.readFile ./shims/git.json));
  }) (drv: {
    src = if builtins.pathExists ./ghcjs/git.json then nixpkgs.fetchgit (builtins.fromJSON (builtins.readFile ./ghcjs/git.json)) else ./ghcjs;
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

  # Tools that are useful for development under both ghc and ghcjs
  generalDevTools = [
    nixpkgs.nodejs
    nixpkgs.curl
    ghc.cabal-install
    ghc.ghcid
    ghc.cabal2nix
#    ghcjs.ghc."socket.io"
  ];

  workOn = haskellPackages: package: (overrideCabal package (drv: {
    buildDepends = (drv.buildDepends or []) ++ generalDevTools;
  })).env;

  workOnMulti = env: packageNames: nixpkgs.runCommand "shell" {
    buildInputs = [
      (env.ghc.withPackages (packageEnv: builtins.concatLists (map (n: packageEnv.${n}.override { mkDerivation = x: builtins.filter (p: builtins.all (nameToAvoid: (p.pname or "") != nameToAvoid) packageNames) (x.buildDepends or []) ++ (x.libraryHaskellDepends or []) ++ (x.executableHaskellDepends or []); }) packageNames)))
    ] ++ generalDevTools;
  } "";

  # The systems that we want to build for on the current system
  cacheTargetSystems = [ "x86_64-linux" "i686-linux" "x86_64-darwin" ];

  isSuffixOf = suffix: s:
    let suffixLen = builtins.stringLength suffix;
    in builtins.substring (builtins.stringLength s - suffixLen) suffixLen s == suffix;

  inherit lib cabal2nixResult;
}
