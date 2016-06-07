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
        reflex = doCheck (self.callPackage ./reflex {});
        reflex-dom = self.callPackage ./reflex-dom {};
        reflex-todomvc = self.callPackage ./reflex-todomvc {};

        # Stick with the pre-gi gtk2hs for now
        gtk2hs-buildtools = self.callPackage (cabal2nixResult "${gtk2hsSrc}/tools") {};
        glib = replaceSrc super.glib "${gtk2hsSrc}/glib" "0.13.2.2";
        gio = replaceSrc super.gio "${gtk2hsSrc}/gio" "0.13.1.1";
        gtk3 = replaceSrc super.gtk3 "${gtk2hsSrc}/gtk" "0.14.2";
        cairo = replaceSrc super.cairo "${gtk2hsSrc}/cairo" "0.13.1.1";
        pango = replaceSrc super.pango "${gtk2hsSrc}/pango" "0.13.1.1";
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

        # Stick with pre-jsaddle ghcjs-dom for now
        ghcjs-dom = self.callPackage ({ mkDerivation, base, glib, gtk3, stdenv, text, transformers, webkitgtk3}: mkDerivation {
          pname = "ghcjs-dom";
          version = "0.2.3.1";
          src = nixpkgs.fetchgit (builtins.fromJSON ''{
            "url": "git://github.com/ryantrinkle/ghcjs-dom",
            "rev": "8a53f7c2510fd15e61e419dea45bc644c7c05e51",
            "sha256": "1kh53p4d8qnlwn6wfajvd6k9jrv0snjzwwflhrxgyv8nfhb7jr1c"
          }'');
          libraryHaskellDepends = [
            base text transformers
          ] ++ (if self.ghc.isGhcjs or false then with self; [
            ghcjs-prim ghc-prim ghcjs-base
          ] else [
            glib gtk3 webkitgtk3
          ]);
          description = "DOM library that supports both GHCJS and WebKitGTK";
          license = stdenv.lib.licenses.mit;
        }) {};

        # https://github.com/ygale/timezone-series/pull/1
        timezone-series = replaceSrc super.timezone-series (nixpkgs.fetchgit (builtins.fromJSON ''{
          "url": "git://github.com/ryantrinkle/timezone-series",
          "rev": "f8dece8c016db6476e2bb0d4f972769a76f6ff40",
          "sha256": "1x7qdjmaahs8hg1azki34aq5h971gqnv2hlyb1y8a1s0ff9ri122"
        }'')) "0.1.5.2";

        # https://github.com/haskell-crypto/cryptonite/issues/88
        cryptonite = overrideCabal super.cryptonite (drv: {
          version = "0.15";
          sha256 = "00y4ga8rbmvlv6m9k4fkjndmb70nhngif9vahghhaxxqpg1gmn5f";
        });

        #TODO: Check this: GHCJS only works with these older versions of haddock
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

        # Jailbreaks
        statistics = dontHaddock super.statistics;
        ref-tf = doJailbreak super.ref-tf;
        deepseq-generics = doJailbreak super.deepseq-generics;
        MonadCatchIO-transformers = doJailbreak super.MonadCatchIO-transformers;
        blaze-builder-enumerator = doJailbreak super.blaze-builder-enumerator;
        diagrams-contrib = doJailbreak super.diagrams-contrib;

        # Failing tests
        ed25519 = dontCheck super.ed25519;
        git = dontCheck super.git;

        # Failing haddocks
        hackage-security = dontHaddock (dontCheck super.hackage-security);
        MemoTrie = dontHaddock super.MemoTrie;
        diagrams-lib = dontHaddock (appendConfigureFlag super.diagrams-lib "--ghc-option=-XConstrainedClassMethods");

        # Miscellaneous fixes
        diagrams-svg = addBuildDepend (doJailbreak super.diagrams-svg) self.lucid-svg;
        cereal = addBuildDepend super.cereal self.fail;
        semigroups = addBuildDepends super.semigroups (with self; [
          hashable
          unordered-containers
          tagged
        ]);

        ########################################################################
        # Fixups for new nixpkgs
        ########################################################################
        language-nix = dontCheck super.language-nix;
        distribution-nixpkgs = dontCheck super.distribution-nixpkgs;

        # The lens tests take WAY too long to run
        lens = dontCheck super.lens;

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
        Cabal = null;
        Cabal_1_24_0_0 = null;
      };
    };
    overrideForGhc7 = haskellPackages: haskellPackages.override {
      overrides = self: super: {
        cabal-install = overrideCabal super.cabal-install (drv: {
          version = "1.22.9.0";
          sha256 = "0h29x4049h7b1678437ajbd0dz5zxxqg4nd4g8y6aqq2fgjkah47";
          revision = null;
          editedCabalFile = null;
        });
        Cabal = overrideCabal super.Cabal (drv: {
          version = "1.22.8.0";
          sha256 = "0km3h156fy2wd5pv23cgvvgp7ifwh6pkfarsxn3hyidnxkfs4hia";
          revision = null;
          editedCabalFile = null;
        });
      };
    };
in rec {
  inherit nixpkgs overrideCabal extendHaskellPackages;
  ghc = overrideForGhc8 (extendHaskellPackages nixpkgs.pkgs.haskell.packages.ghc801);
  ghc7 = overrideForGhc7 (extendHaskellPackages nixpkgs.pkgs.haskell.packages.ghc7103);
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
