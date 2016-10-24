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
    filterGit = builtins.filterSource (path: type: builtins.any (x: x == baseNameOf path) [".git" "default.nix" "shell.nix"]);
    # All imports of sources need to go here, so that they can be explicitly cached
    sources = {
      gtk2hs = nixpkgs.fetchFromGitHub {
        owner = "gtk2hs";
        repo = "gtk2hs";
        rev = "eee61d84edf1dd44f8d380d7d7cae2405de50124";
        sha256 = "12i53grimni0dyjqjydl120z5amcn668w4pfhl8dxscjh4a0l5nb";
      };
      webkitgtk3 = nixpkgs.fetchFromGitHub {
        owner = "ryantrinkle";
        repo = "webkit";
        rev = "be8046844e8108f88407a2e5548d0c19911cb012";
        sha256 = "1wdkl55m4l9crkbsw8azl4jwqd8rjjkzlrvj8pmb9b48jxcpq7ml";
      };
      webkitgtk3-javascriptcore = nixpkgs.fetchFromGitHub {
        owner = "ryantrinkle";
        repo = "webkit-javascriptcore";
        rev = "f3f4a05754ee0d76511ab7aaa6d080b4212d80e8";
        sha256 = "17xj6nbny9zbqgym5jmx47xzb96g4wsavr5brq03vh4lk3kx3jvc";
      };
      timezone-series = nixpkgs.fetchFromGitHub {
        owner = "ryantrinkle";
        repo = "timezone-series";
        rev = "f8dece8c016db6476e2bb0d4f972769a76f6ff40";
        sha256 = "0j2bxzi102ay4s0vc39vi9xlny7fgsjv379pibdcfzsd6k540517";
      };
      ghcjs-boot = if builtins.pathExists ./ghcjs-boot/git.json then nixpkgs.fetchgit (builtins.fromJSON (builtins.readFile ./ghcjs-boot/git.json)) else {
        name = "ghcjs-boot";
        outPath = filterGit ./ghcjs-boot;
      };
      shims = if builtins.pathExists ./shims/github.json then nixpkgs.fetchFromGitHub (builtins.fromJSON (builtins.readFile ./shims/github.json)) else filterGit ./shims;
      ghcjs = if builtins.pathExists ./ghcjs/github.json then nixpkgs.fetchFromGitHub (builtins.fromJSON (builtins.readFile ./ghcjs/github.json)) else filterGit ./ghcjs;
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
        gtk2hs-buildtools = self.callPackage (cabal2nixResult "${sources.gtk2hs}/tools") {};
        glib = replaceSrc super.glib "${sources.gtk2hs}/glib" "0.13.2.2";
        gio = replaceSrc super.gio "${sources.gtk2hs}/gio" "0.13.1.1";
        gtk3 = replaceSrc super.gtk3 "${sources.gtk2hs}/gtk" "0.14.2";
        cairo = replaceSrc super.cairo "${sources.gtk2hs}/cairo" "0.13.1.1";
        pango = replaceSrc super.pango "${sources.gtk2hs}/pango" "0.13.1.1";
        webkitgtk3 = self.callPackage (cabal2nixResult sources.webkitgtk3) { webkit = nixpkgs.webkitgtk24x; };
        webkitgtk3-javascriptcore = self.callPackage (cabal2nixResult sources.webkitgtk3-javascriptcore) { webkit = nixpkgs.webkitgtk24x; };

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

        # https://github.com/ygale/timezone-series/pull/1
        timezone-series = self.callPackage (cabal2nixResult sources.timezone-series) {};

        # Jailbreaks
        ref-tf = doJailbreak super.ref-tf;
        deepseq-generics = doJailbreak super.deepseq-generics;
        MonadCatchIO-transformers = doJailbreak super.MonadCatchIO-transformers;
        blaze-builder-enumerator = doJailbreak super.blaze-builder-enumerator;
        diagrams-contrib = doJailbreak super.diagrams-contrib;

        vector-algorithms = overrideCabal super.vector-algorithms (drv: {
          libraryHaskellDepends = drv.libraryHaskellDepends ++ [ self.mtl self.mwc-random ];
        });

        Glob = overrideCabal super.Glob (drv: {
          libraryHaskellDepends = drv.libraryHaskellDepends ++ [ self.semigroups ];
        });

        # Failing tests
        ed25519 = dontCheck super.ed25519;
        git = dontCheck super.git;

        # Failing haddocks
        MemoTrie = dontHaddock super.MemoTrie;
        diagrams-lib = dontHaddock (appendConfigureFlag super.diagrams-lib "--ghc-option=-XConstrainedClassMethods");
        hackage-security = dontHaddock (dontCheck super.hackage-security);
        statistics = dontHaddock super.statistics;

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
    overrideForGhc8 = haskellPackages: haskellPackages.override {
      overrides = self: super: {
        Cabal = null;
        Cabal_1_24_0_0 = null;
        ghcjs-prim = null;
      };
    };
    overrideForGhc7 = haskellPackages: haskellPackages.override {
      overrides = self: super: {
        cabal-install = self.cabal-install_1_22_9_0;
        Cabal = self.Cabal_1_22_8_0;
        cereal = dontCheck super.cereal; # cereal's test suite requires a newer version of bytestring than this haskell environment provides
      };
    };
    overrideForGhc7_8 = haskellPackages: (overrideForGhc7 haskellPackages).override {
      overrides = self: super: {
        mkDerivation = drv: super.mkDerivation (drv // {
          enableSplitObjs = false; # Split objects with template haskell doesn't work on ghc 7.8
        });
        MemoTrie = addBuildDepend super.MemoTrie self.void;
        generic-deriving = dontHaddock super.generic-deriving;
        aeson = overrideCabal super.aeson (drv: {
          revision = "1";
          editedCabalFile = "680affa9ec12880014875ce8281efb2407efde69c30e9a82654e973e5dc2c8a1";
          buildDepends = (drv.buildDepends or []) ++ [
            self.nats
            self.semigroups
          ];
        });
        bifunctors = dontHaddock super.bifunctors;
        cereal = dontCheck super.cereal; # cereal's test suite requires a newer version of bytestring than this haskell environment provides
      };
    };
in rec {
  overrideForGhcjs = haskellPackages: haskellPackages.override {
    overrides = self: super: {
      mkDerivation = drv: super.mkDerivation.override {
        hscolour = ghc.hscolour;
      } (drv // {
        doHaddock = false;
      });

      ghcWithPackages = selectFrom: self.callPackage "${nixpkgs.path}/pkgs/development/haskell-modules/with-packages-wrapper.nix" {
        inherit (self) llvmPackages;
        haskellPackages = self;
        packages = selectFrom self;
        #ghcLibdir = let x = "${self.ghc.ghcPackages.ghcWithPackages selectFrom}/lib/${self.ghc.ghcPackages.ghc.name}"; in builtins.trace x x;
        ghcLibdir = "${ghc.ghcWithPackages (p: [ p.reflex ])}/lib/${ghc.ghc.name}";
      };

      ghc = super.ghc // {
        withPackages = self.ghcWithPackages;
      };

      reflex = overrideCabal super.reflex (drv: {
        configureFlags = [
          "--ghcjs-option=-dynamic-too"
        ];
      });
    };
  };
  inherit nixpkgs overrideCabal extendHaskellPackages;
  ghc = overrideForGhc8 (extendHaskellPackages nixpkgs.pkgs.haskell.packages.ghc801);
  ghc7 = overrideForGhc7 (extendHaskellPackages nixpkgs.pkgs.haskell.packages.ghc7103);
  ghc7_8 = overrideForGhc7_8 (extendHaskellPackages nixpkgs.pkgs.haskell.packages.ghc784);
  stage2Script = nixpkgs.runCommand "stage2.nix" {
    buildCommand = ''
      ${nixpkgs.path}/pkgs/development/compilers/ghcjs/gen-stage2.rb "${sources.ghcjs-boot}" >"$out"
    '';
    buildInputs = with nixpkgs; [
      ruby cabal2nix
    ];
  } "";
  setGhcLibdir = ghcLibdir: inputGhcjs:
    let libDir = "$out/lib/ghcjs-${inputGhcjs.version}";
        ghcLibdirLink = nixpkgs.stdenv.mkDerivation {
          name = "ghc_libdir";
          inherit ghcLibdir;
          buildCommand = ''
            mkdir -p ${libDir}
            echo "$ghcLibdir" > ${libDir}/ghc_libdir_override
          '';
        };
    in inputGhcjs // {
    outPath = nixpkgs.buildEnv {
      inherit (inputGhcjs) name;
      paths = [ inputGhcjs ghcLibdirLink ];
      postBuild = ''
        mv ${libDir}/ghc_libdir_override ${libDir}/ghc_libdir
      '';
    };
  };
  ghcjsCompiler = (overrideCabal (ghc.callPackage "${nixpkgs.path}/pkgs/development/compilers/ghcjs" {
    bootPkgs = ghc;
    ghcjsBootSrc = sources.ghcjs-boot;
    shims = sources.shims;
    ghcLibdir = "${ghc.ghcWithPackages (p: [ p.reflex ])}/lib/${ghc.ghc.name}";
  }) (drv: {
    src = sources.ghcjs;
  })) // {
    mkStage2 = import stage2Script {
      ghcjsBoot = sources.ghcjs-boot;
    };
    stage1Packages = [
      "array"
      "base"
      "binary"
      "bytestring"
      "containers"
      "deepseq"
      "directory"
      "filepath"
      "ghc-boot"
      "ghc-boot-th"
      "ghc-prim"
      "ghci"
      "ghcjs-prim"
      "ghcjs-th"
      "integer-gmp"
      "pretty"
      "primitive"
      "process"
      "rts"
      "template-haskell"
      "time"
      "transformers"
      "unix"
    ];
  };
  ghcjsPackages = nixpkgs.callPackage "${nixpkgs.path}/pkgs/development/haskell-modules" {
    ghc = ghcjsCompiler;
    packageSetConfig = nixpkgs.callPackage "${nixpkgs.path}/pkgs/development/haskell-modules/configuration-ghcjs.nix" { };
  };

  ghcjs = overrideForGhcjs (extendHaskellPackages ghcjsPackages);
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
  generalDevTools = haskellPackages:
    let nativeHaskellPackages = if haskellPackages.ghc.isGhcjs or false then ghc else haskellPackages;
    in [
    nixpkgs.nodejs
    nixpkgs.curl
    nixpkgs.cabal2nix
    nixpkgs.nix-prefetch-scripts
    nativeHaskellPackages.cabal-install
    nativeHaskellPackages.ghcid
    nativeHaskellPackages.hlint
  ] ++ (if builtins.compareVersions haskellPackages.ghc.version "7.10" >= 0 then [
    nativeHaskellPackages.stylish-haskell # Recent stylish-haskell only builds with AMP in place
  ] else []);

  nativeHaskellPackages = haskellPackages:
    if haskellPackages.isGhcjs or false
    then haskellPackages.ghc
    else haskellPackages;

  workOn = haskellPackages: package: (overrideCabal package (drv: {
    buildDepends = (drv.buildDepends or []) ++ generalDevTools (nativeHaskellPackages haskellPackages);
  })).env;

  workOnMulti = env: packageNames: nixpkgs.runCommand "shell" {
    buildInputs = [
      (env.ghc.withPackages (packageEnv: builtins.concatLists (map (n: packageEnv.${n}.override { mkDerivation = x: builtins.filter (p: builtins.all (nameToAvoid: (p.pname or "") != nameToAvoid) packageNames) (x.buildDepends or []) ++ (x.libraryHaskellDepends or []) ++ (x.executableHaskellDepends or []); }) packageNames)))
    ] ++ generalDevTools env;
  } "";

  # A simple derivation that just creates a file with the names of all of its inputs.  If built, it will have a runtime dependency on all of the given build inputs.
  pinBuildInputs = drvName: buildInputs: otherDeps: nixpkgs.runCommand drvName {
    buildCommand = ''
      mkdir "$out"
      echo "$propagatedBuildInputs $buildInputs $nativeBuildInputs $propagatedNativeBuildInputs $otherDeps" > "$out/deps"
    '';
    inherit buildInputs otherDeps;
  } "";

  # The systems that we want to build for on the current system
  cacheTargetSystems = [
    "x86_64-linux"
    "i686-linux"
    "x86_64-darwin"
  ];

  isSuffixOf = suffix: s:
    let suffixLen = builtins.stringLength suffix;
    in builtins.substring (builtins.stringLength s - suffixLen) suffixLen s == suffix;

  inherit lib cabal2nixResult sources;
}
