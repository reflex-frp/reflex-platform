{ nixpkgsFunc ? import ./nixpkgs
, system ? null
, config ? null
}:
let nixpkgs = nixpkgsFunc ({
      config = {
        allowUnfree = true;
        allowBroken = true; # GHCJS is marked broken in 011c149ed5e5a336c3039f0b9d4303020cff1d86
      } // (if config == null then {} else config);
    } // (
      if system == null then {} else { inherit system; }
    ));
    inherit (nixpkgs.haskell) lib;
in with lib;
let overrideCabal = pkg: f: if pkg == null then null else lib.overrideCabal pkg f;
    hspecGit = nixpkgs.fetchgit {
      url = git://github.com/ryantrinkle/hspec;
      rev = "937c0ae61d70dcd71c35a170b800c30f14a5bc9c";
      sha256 = "1819d5b3f973b432339256ba783b33ada691a785d059e83009e5e2edc6178f6d";
    };
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

      # The lens tests take WAY too long to run
      lens = dontCheck super.lens;
    };
in rec {
  inherit nixpkgs overrideCabal overrides;
  ghc = nixpkgs.pkgs.haskell.packages.ghc7103.override {
    inherit overrides;
  };
  ghcjsCompiler = overrideCabal (ghc.callPackage "${nixpkgs.path}/pkgs/development/compilers/ghcjs" {
    bootPkgs = ghc;
    ghcjsBootSrc = nixpkgs.fetchgit {
      url = git://github.com/ghcjs/ghcjs-boot.git;
      rev = "97dea5c4145bf80a1e7cffeb1ecd4d0ecacd5a2f";
      sha256 = "1295429501c0c1a7504b0b0215f12928dc35c4f673fd159de94dd0924afdf2b1";
      fetchSubmodules = true;
    };
    shims = nixpkgs.fetchgit {
      url = git://github.com/ghcjs/shims.git;
      rev = "4df1808d03117ddcd45f276f0ddd85c73e59506a";
      sha256 = "aa3515cc0f52ed0e9a14310ac66e8b80a024ce88099c21fedd18fb81eb255e59";
    };
  }) (drv: {
    src = nixpkgs.fetchgit {
      url = git://github.com/ghcjs/ghcjs.git;
      rev = "13a99c6da40e3700e070e430d4c0f2ea96217b24";
      sha256 = "6e6c34f98092032203ff775b108594bee68fa73510872824daeaa1d71a738a83";
    };
  });
  ghcjs = nixpkgs.pkgs.haskell.packages.ghcjs.override {
    ghc = ghcjsCompiler;
    inherit overrides;
  };

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
    ghc.cabal-install
    ghc.ghcid
  ];

  workOn = package: (overrideCabal package (drv: {
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

  inherit lib;
}
