{ nixpkgsFunc ? import ./nixpkgs
, system ? null
, config ? null
, enableLibraryProfiling ? false
}:
let nixpkgs = nixpkgsFunc ({
      config = {
        allowUnfree = true;
        allowBroken = true; # GHCJS is marked broken in 011c149ed5e5a336c3039f0b9d4303020cff1d86
        packageOverrides = pkgs: rec {
          mercurial = pkgs.mercurial.override {
            hg-git = "";
            dulwich = "";
          };
        };
      } // (if config == null then {} else config);
    } // (
      if system == null then {} else { inherit system; }
    ));
    lib = import "${nixpkgs.path}/pkgs/development/haskell-modules/lib.nix" { pkgs = nixpkgs; };
    gtk2hsSrc = nixpkgs.fetchFromGitHub {
      owner = "gtk2hs";
      repo = "gtk2hs";
      rev = "eee61d84edf1dd44f8d380d7d7cae2405de50124";
      sha256 = "12i53grimni0dyjqjydl120z5amcn668w4pfhl8dxscjh4a0l5nb";
    };
in with lib;
let overrideCabal = pkg: f: if pkg == null then null else lib.overrideCabal pkg f;
    replaceSrc = pkg: src: version: overrideCabal pkg (drv: {
      inherit src version;
      sha256 = null;
      revision = null;
      editedCabalFile = null;
    });
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

        # Stick with the pre-gi gtk2hs for now
        gtk2hs-buildtools = self.callPackage (cabal2nixResult "${gtk2hsSrc}/tools") {};
        glib = replaceSrc super.glib "${gtk2hsSrc}/glib" "0.13.2.2";
        gio = replaceSrc super.gio "${gtk2hsSrc}/gio" "0.13.1.1";
        gtk3 = replaceSrc super.gtk3 "${gtk2hsSrc}/gtk" "0.14.2";
        cairo = replaceSrc super.cairo "${gtk2hsSrc}/cairo" "0.13.1.1";
        pango = replaceSrc super.pango "${gtk2hsSrc}/pango" "0.13.1.1";
        webkitgtk3 = self.callPackage (cabal2nixResult (nixpkgs.fetchFromGitHub {
          owner = "ryantrinkle";
          repo = "webkit";
          rev = "be8046844e8108f88407a2e5548d0c19911cb012";
          sha256 = "1wdkl55m4l9crkbsw8azl4jwqd8rjjkzlrvj8pmb9b48jxcpq7ml";
        })) { webkit = nixpkgs.webkitgtk24x; };
        webkitgtk3-javascriptcore = self.callPackage (cabal2nixResult (nixpkgs.fetchFromGitHub {
          owner = "ryantrinkle";
          repo = "webkit-javascriptcore";
          rev = "f3f4a05754ee0d76511ab7aaa6d080b4212d80e8";
          sha256 = "17xj6nbny9zbqgym5jmx47xzb96g4wsavr5brq03vh4lk3kx3jvc";
        })) { webkit = nixpkgs.webkitgtk24x; };

        # Stick with pre-jsaddle ghcjs-dom for now
        ghcjs-dom = self.callPackage ({ mkDerivation, base, glib, gtk3, stdenv, text, transformers, webkitgtk3 }: mkDerivation {
          pname = "ghcjs-dom";
          version = "0.2.3.1";
          sha256 = "0fgfmhzlz960vvm2l8a441yv9nv95h6wz32815x71mbaskff7pnz";
          libraryHaskellDepends = [
            base text transformers
          ] ++ (if self.ghc.isGhcjs or false then with self; [
            ghcjs-prim ghc-prim ghcjs-base
          ] else [
            glib gtk3 webkitgtk3
          ]);
          preConfigure = ''
            sed -i 's/\(transformers .*\)<0.5/\1<0.6/' *.cabal
          '';
          description = "DOM library that supports both GHCJS and WebKitGTK";
          license = stdenv.lib.licenses.mit;
        }) {};

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

        Glob = overrideCabal super.Glob (drv: {
          libraryHaskellDepends = drv.libraryHaskellDepends ++ [ self.semigroups ];
        });

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

      } // (if enableLibraryProfiling then {
        mkDerivation = expr: super.mkDerivation (expr // { enableLibraryProfiling = true; });
      } else {});
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
  ghc = extendHaskellPackages nixpkgs.pkgs.haskell.packages.ghc801;
  ghc7 = overrideForGhc7 (extendHaskellPackages nixpkgs.pkgs.haskell.packages.ghc7103);
  ghcjsCompiler = overrideCabal (ghc7.callPackage "${nixpkgs.path}/pkgs/development/compilers/ghcjs" {
    bootPkgs = ghc7;
    ghcjsBootSrc = nixpkgs.fetchgit (builtins.fromJSON (builtins.readFile ./ghcjs-boot/git.json));
    shims = nixpkgs.fetchFromGitHub (builtins.fromJSON (builtins.readFile ./shims/github.json));
  }) (drv: {
    src = if builtins.pathExists ./ghcjs/github.json then nixpkgs.fetchFromGitHub (builtins.fromJSON (builtins.readFile ./ghcjs/github.json)) else ./ghcjs;
  });
  ghcjsPackages = nixpkgs.callPackage "${nixpkgs.path}/pkgs/development/haskell-modules" {
    ghc = ghcjsCompiler;
    packageSetConfig = nixpkgs.callPackage "${nixpkgs.path}/pkgs/development/haskell-modules/configuration-ghcjs.nix" { };
  };

  ghcjs = extendHaskellPackages ghcjsPackages;
  platforms = [ "ghcjs" "ghc" ];

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
    ghc.hlint
    ghc.stylish-haskell
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
