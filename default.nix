{ nixpkgsFunc ? import ./nixpkgs
, system ? null
, config ? null
, enableLibraryProfiling ? false
, useReflexOptimizer ? false
, useTextJSString ? true
}:
let nixpkgs = nixpkgsFunc ({
      config = {
        allowUnfree = true;
        allowBroken = true; # GHCJS is marked broken in 011c149ed5e5a336c3039f0b9d4303020cff1d86
        packageOverrides = pkgs: {
          webkitgtk = pkgs.webkitgtk214x;
          osx_sdk = pkgs.callPackage ({ stdenv, fetchzip }:
            let version = "10.11";
	    in stdenv.mkDerivation rec {
	    name = "MacOSX10.11.sdk";

	    src = fetchzip {
	      url = "https://github.com/phracker/MacOSX-SDKs/releases/download/MacOSX10.11.sdk/MacOSX10.11.sdk.tar.xz";
	      sha256 = "132vz288l6pk7ci49fcvkkmci47w451ggidh3sarm1f9m7sg7b1k";
	    };

	    unpackPhase    = "true";
	    configurePhase = "true";
	    buildPhase     = "true";
	    setupHook = ./setup-hook.sh;

	    installPhase = ''
	      mkdir -p $out/Developer/SDKs/
	      echo "Source is: $src"
	      cp -r $src/* $out/Developer/SDKs/
	    '';

	    meta = with stdenv.lib; {
	      description = "The Mac OS ${version} SDK";
	      maintainers = with maintainers; [ copumpkin ];
	      platforms   = platforms.darwin;
	      license     = licenses.unfree;
	    };
	  }) {};
        };
      } // (if config == null then {} else config);
    } // (
      if system == null then {} else { inherit system; }
    ));
    lib = import (nixpkgs.path + "/pkgs/development/haskell-modules/lib.nix") { pkgs = nixpkgs; };
    filterGit = builtins.filterSource (path: type: !(builtins.any (x: x == baseNameOf path) [".git" "default.nix" "shell.nix"]));
    # All imports of sources need to go here, so that they can be explicitly cached
    sources = {
      intero = nixpkgs.fetchFromGitHub {
        owner = "commercialhaskell";
        repo = "intero";
        rev = "5378bb637c76c48eca64ccda0c855f7557aecb60";
        sha256 = "1vgmbs790l8z90bk8sib3xvli06p1nkrjnnvlnhsjzkkpxynf2nf";
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
    addReflexOptimizerFlag = if useReflexOptimizer
      then drv: appendConfigureFlag drv "-fuse-reflex-optimizer"
      else drv: drv;
    extendHaskellPackages = haskellPackages: makeRecursivelyOverridable haskellPackages {
      overrides = self: super:
        let reflexDom = import ./reflex-dom self;
        in {
        ########################################################################
        # Reflex packages
        ########################################################################
        reflex = addReflexOptimizerFlag (self.callPackage ./reflex {});
        reflex-dom = addReflexOptimizerFlag reflexDom.reflex-dom;
        reflex-dom-core = addReflexOptimizerFlag reflexDom.reflex-dom-core;
        reflex-todomvc = self.callPackage ./reflex-todomvc {};

#        Cabal = self.Cabal_1_24_2_0;

        gi-atk = super.gi-atk_2_0_11;
        gi-cairo = super.gi-cairo_1_0_11;
        gi-gdk = super.gi-gdk_3_0_11;
        gi-gdkpixbuf = super.gi-gdkpixbuf_2_0_11;
        gi-gio = super.gi-gio_2_0_11;
        gi-glib = super.gi-glib_2_0_11;
        gi-gobject = super.gi-gobject_2_0_11;
        gi-gtk = super.gi-gtk_3_0_11;
        gi-javascriptcore = super.gi-javascriptcore_4_0_11;
        gi-pango = super.gi-pango_1_0_11;
        gi-soup = super.gi-soup_2_4_11;
        gi-webkit = super.gi-webkit_3_0_11;
        gi-webkit2 = super.gi-webkit2.override {
          webkit2gtk = nixpkgs.webkitgtk214x;
        };
        haskell-gi = super.haskell-gi_0_20;
        haskell-gi-base = super.haskell-gi-base_0_20;
        webkit2gtk3-javascriptcore = super.webkit2gtk3-javascriptcore.override {
          webkit2gtk = nixpkgs.webkitgtk214x;
        };
        gtk2hs-buildtools = doJailbreak super.gtk2hs-buildtools;
        shelly = overrideCabal (doJailbreak super.shelly) (drv: {
          preConfigure = (drv.preConfigure or "") + ''
            sed -i 's/base .*<.*4\.9\.1/base/' *.cabal
            sed -i 's/\(default (T.Text)\)/-- \1/' src/Shelly/Pipe.hs
          '';
        });
        jsaddle-wkwebview = overrideCabal super.jsaddle-wkwebview (drv: {
          version = "0.8.2.0";
          sha256 = "1dnzs94s2kw997rw4bwkmlpbsbwlgqhznarg91k73vb8kc8nhyda";
	  preBuild = ''
            sed -i 's/\(windowStyleMask =\).*;/\1 0xf;/g' cbits-cocoa/AppDelegate.m
          '';
          libraryHaskellDepends = (drv.libraryHaskellDepends or []) ++ [
            nixpkgs.osx_sdk
            nixpkgs.darwin.libobjc
            nixpkgs.darwin.apple_sdk.libs.xpc
          ];
        });
        cabal-macosx = overrideCabal super.cabal-macosx (drv: {
          src = nixpkgs.fetchFromGitHub {
            owner = "hamishmack";
            repo = "cabal-macosx";
            rev = "901a76e59fddb83b3bb38d44374528d24c4f0785";
            sha256 = "0azj9rrmc3k0s5347faizfmxfsqyp0pxnr9gxp7z38jg9y8ddhh1";
          };
        });

        intero = replaceSrc super.intero "${sources.intero}" "0.1.18";

        dependent-map = overrideCabal super.dependent-map (drv: {
          version = "0.2.4.0";
          sha256 = "0il2naf6gdkvkhscvqd8kg9v911vdhqp9h10z5546mninnyrdcsx";
        });
        dependent-sum = overrideCabal super.dependent-sum (drv: {
          version = "0.4";
          sha256 = "07hs9s78wiybwjwkal2yq65hdavq0gg1h2ld7wbph61s2nsfrpm8";
        });
        dependent-sum-template = doJailbreak super.dependent-sum-template;

        jsaddle-dom = overrideCabal super.jsaddle-dom (drv: {
          version = "0.7.1.0";
          sha256 = "0fnm0s7kh3bbsy2rpkphxfncfw8c9dkvcbqnd8i419lsrpfafgp9";
        });
        jsaddle-webkit2gtk = overrideCabal super.jsaddle-webkit2gtk (drv: {
          preConfigure = (drv.preConfigure or "") + ''
            sed -i 's/directory .*<.*1.3/directory/' *.cabal
          '';
        });

        # https://github.com/ygale/timezone-series/pull/1
        timezone-series = self.callPackage (cabal2nixResult sources.timezone-series) {};

        # Jailbreaks
        ref-tf = doJailbreak super.ref-tf;
        deepseq-generics = doJailbreak super.deepseq-generics;
        MonadCatchIO-transformers = doJailbreak super.MonadCatchIO-transformers;
        blaze-builder-enumerator = doJailbreak super.blaze-builder-enumerator;
        diagrams-contrib = doJailbreak super.diagrams-contrib;
        cases = doJailbreak super.cases; # The test suite's bounds on HTF are too strict

        vector-algorithms = overrideCabal super.vector-algorithms (drv: {
          libraryHaskellDepends = drv.libraryHaskellDepends ++ [ self.mtl self.mwc-random ];
        });

        Glob = overrideCabal super.Glob (drv: {
          libraryHaskellDepends = drv.libraryHaskellDepends ++ [ self.semigroups ];
        });

        # keycode-0.2 has a bug on firefox
        keycode = overrideCabal super.keycode (drv: {
          version = "0.2.2";
          sha256 = "046k8d1h5wwadf5z4pppjkc3g7v2zxlzb06s1xgixc42y5y41yan";
          revision = null;
          editedCabalFile = null;
        });

        # Failing tests
        ed25519 = dontCheck super.ed25519;
        git = dontCheck super.git;

        # Failing haddocks
        MemoTrie = dontHaddock super.MemoTrie;
        diagrams-lib = dontHaddock (appendConfigureFlag super.diagrams-lib "--ghc-option=-XConstrainedClassMethods");
        hackage-security = dontHaddock (dontCheck super.hackage-security);
        # statistics = dontHaddock super.statistics;

        # Miscellaneous fixes
        diagrams-svg = addBuildDepend (doJailbreak super.diagrams-svg) self.lucid-svg;
        cereal = addBuildDepend super.cereal self.fail;
        semigroups = addBuildDepends super.semigroups (with self; [
          hashable
          unordered-containers
          tagged
        ]);
        packunused = doJailbreak (replaceSrc super.packunused (nixpkgs.fetchFromGitHub {
          owner = "hvr";
          repo = "packunused";
          rev = "60b305a3e8f838aa92cff6265979108405bfa347";
          sha256 = "0qb96kkc4v2wg7jy7iyc7v1b1lxzk7xvkgjrw3s81gbx9b3slllb";
        }) "0.1.1.4");
        stylish-haskell = doJailbreak super.stylish-haskell;

        haddock-api = replaceSrc super.haddock-api ((nixpkgs.fetchFromGitHub {
          owner = "haskell";
          repo = "haddock";
          rev = "240bc38b94ed2d0af27333b23392d03eeb615e82";
          sha256 = "198va5xq6prp626prfxf1qlmw4pahzkqgr8dbxmpa323vdq8zlix";
        }) + "/haddock-api") "2.17.3";

        ########################################################################
        # Fixups for new nixpkgs
        ########################################################################
        language-nix = dontCheck super.language-nix;
        distribution-nixpkgs = dontCheck super.distribution-nixpkgs;

        # The lens tests take WAY too long to run
        lens = dontCheck super.lens;

      } // (if enableLibraryProfiling && !(super.ghc.isGhcjs or false) then {
        mkDerivation = expr: super.mkDerivation (expr // { enableLibraryProfiling = true; });
      } else {});
    };
    overrideForGhc8 = haskellPackages: haskellPackages.override {
      overrides = self: super: {
        ghcjs-prim = null;
        ghcjs-json = null;
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
    overridesForTextJSString = self: super: {
      text = overrideCabal super.text (drv: {
        src = nixpkgs.fetchFromGitHub {
          owner = "luigy";
          repo = "text";
          rev = "90d36ec1a0be48c25ffc15ddf66c83971e519f34";
          sha256 = "0d0dlp06pdww1wc9a04nfzfs578ag81knlkpmksc3pz5h2364nak";
        };
        buildDepends = (drv.buildDepends or []) ++ [
          self.ghcjs-base
        ];
      });
      jsaddle = overrideCabal super.jsaddle (drv: {
        patches = (drv.patches or []) ++ [
          ./jsaddle-text-jsstring.patch
        ];
        buildDepends = (drv.buildDepends or []) ++ [
          self.ghcjs-json
        ];
      });
      ghcjs-json = self.callPackage (cabal2nixResult (nixpkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "ghcjs-json";
        rev = "3a6e1e949aced800d32e0683a107f5387295f3a6";
        sha256 = "1pjsvyvy6ac3358db19iwgbmsmm0si2hzh2ja1hclq43q6d80yij";
      })) {};
      ghcjs-base = overrideCabal super.ghcjs-base (drv: {
        src = nixpkgs.fetchFromGitHub {
          owner = "luigy";
          repo = "ghcjs-base";
          rev = "8569f5d541aa846f2130ff789d19bcd55ea41d2a";
          sha256 = "1b1fyqgn7jxh4rawgxidacafg6jwfdfcidyh93z6a6lhmm5qaq3n";
        };
        libraryHaskellDepends = with self; [
          base bytestring containers deepseq dlist ghc-prim
          ghcjs-prim integer-gmp primitive time
          transformers vector
        ];
      });
      attoparsec = overrideCabal super.attoparsec (drv: {
        src = nixpkgs.fetchFromGitHub {
          owner = "luigy";
          repo = "attoparsec";
          rev = "e766a754811042f061b6b4498137d2ad28e207a8";
          sha256 = "106fn187hw9z3bidbkp7r4wafmhk7g2iv2k0hybirv63f8727x3x";
        };
      });
      hashable = overrideCabal super.hashable (drv: {
        src = nixpkgs.fetchFromGitHub {
          owner = "luigy";
          repo = "hashable";
          rev = "97a6fc77b028b4b3a7310a5c2897b8611e518870";
          sha256 = "1rl55p5y0mm8a7hxlfzhhgnnciw2h63ilxdaag3h7ypdx4bfd6rs";
        };
      });
      conduit-extra = overrideCabal super.conduit-extra (drv: {
        src = "${nixpkgs.fetchFromGitHub {
          owner = "luigy";
          repo = "conduit";
          rev = "aeb20e4eb7f7bfc07ec401c82821cbb04018b571";
          sha256 = "10kz2m2yxyhk46xdglj7wdn5ba2swqzhyznxasj0jvnjcnv3jriw";
        }}/conduit-extra";
      });
    };
in let this = rec {
  overrideForGhcjs = haskellPackages: haskellPackages.override {
    overrides = self: super: {
      mkDerivation = drv: super.mkDerivation.override {
        hscolour = ghc.hscolour;
      } (drv // {
        doHaddock = false;
      });

      ghcWithPackages = selectFrom: self.callPackage (nixpkgs.path + "/pkgs/development/haskell-modules/with-packages-wrapper.nix") {
        inherit (self) llvmPackages;
        haskellPackages = self;
        packages = selectFrom self;
        ${if useReflexOptimizer then "ghcLibdir" else null} = "${ghc.ghcWithPackages (p: [ p.reflex ])}/lib/${ghc.ghc.name}";
      };

      ghc = super.ghc // {
        withPackages = self.ghcWithPackages;
      };

    } // (if useTextJSString then overridesForTextJSString self super else {});
  };
  inherit nixpkgs overrideCabal extendHaskellPackages;
  ghc = overrideForGhc8 (extendHaskellPackages nixpkgs.pkgs.haskell.packages.ghc802);
  ghc7 = overrideForGhc7 (extendHaskellPackages nixpkgs.pkgs.haskell.packages.ghc7103);
  ghc7_8 = overrideForGhc7_8 (extendHaskellPackages nixpkgs.pkgs.haskell.packages.ghc784);
  stage2Script = nixpkgs.runCommand "stage2.nix" {
    GEN_STAGE2 = builtins.readFile (nixpkgs.path + "/pkgs/development/compilers/ghcjs/gen-stage2.rb");
    buildCommand = ''
      echo "$GEN_STAGE2" > gen-stage2.rb && chmod +x gen-stage2.rb
      patchShebangs .
      ./gen-stage2.rb "${sources.ghcjs-boot}" >"$out"
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
  ghcjsCompiler = (overrideCabal (ghc.callPackage (nixpkgs.path + "/pkgs/development/compilers/ghcjs/base.nix") {
    bootPkgs = ghc;
    ghcjsBootSrc = sources.ghcjs-boot;
    shims = sources.shims;
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
  ghcjsPackages = nixpkgs.callPackage (nixpkgs.path + "/pkgs/development/haskell-modules") {
    ghc = ghcjsCompiler;
    packageSetConfig = nixpkgs.callPackage (nixpkgs.path + "/pkgs/development/haskell-modules/configuration-ghcjs.nix") { };
  };

  ghcjs = overrideForGhcjs (extendHaskellPackages ghcjsPackages);
  platforms = [ "ghc" ];

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
    nativeHaskellPackages.Cabal
    nativeHaskellPackages.cabal-install
    nativeHaskellPackages.ghcid
    nativeHaskellPackages.hlint
    nativeHaskellPackages.packunused
    nixpkgs.cabal2nix
    nixpkgs.curl
    nixpkgs.nix-prefetch-scripts
    nixpkgs.nodejs
    nixpkgs.pkgconfig
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
    # "i686-linux" Broken on ghc 8.0.2-rc2
    "x86_64-darwin"
  ];

  isSuffixOf = suffix: s:
    let suffixLen = builtins.stringLength suffix;
    in builtins.substring (builtins.stringLength s - suffixLen) suffixLen s == suffix;

  reflexEnv = platform: (builtins.getAttr platform this).ghcWithPackages (p: import ./packages.nix { haskellPackages = p; inherit platform; });

  tryReflexPackages = generalDevTools ghc ++ builtins.map reflexEnv platforms;

  demoVM = (import "${nixpkgs.path}/nixos" {
    configuration = {
      imports = [
        "${nixpkgs.path}/nixos/modules/virtualisation/virtualbox-image.nix"
        "${nixpkgs.path}/nixos/modules/profiles/demo.nix"
      ];
      environment.systemPackages = tryReflexPackages;
    };
  }).config.system.build.virtualBoxOVA;

  inherit lib cabal2nixResult sources;
}; in this
