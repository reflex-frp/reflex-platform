{ useTextJSString ? false, useFastWeak ? true }: final: prev: let
 installDeps = targetPrefix:
      ''
      $out/bin/${targetPrefix}ghc-pkg --version
      for P in $($out/bin/${targetPrefix}ghc-pkg list --simple-output | sed 's/-[0-9][0-9.]*//g'); do
        mkdir -p $out/exactDeps/$P
        touch $out/exactDeps/$P/configure-flags
        touch $out/exactDeps/$P/cabal.config

        if id=$($out/bin/${targetPrefix}ghc-pkg field $P id --simple-output); then
          echo "--dependency=$P=$id" >> $out/exactDeps/$P/configure-flags
        elif id=$($out/bin/${targetPrefix}ghc-pkg field "z-$P-z-*" id --simple-output); then
          name=$($out/bin/${targetPrefix}ghc-pkg field "z-$P-z-*" name --simple-output)
          # so we are dealing with a sublib. As we build sublibs separately, the above
          # query should be safe.
          echo "--dependency=''${name#z-$P-z-}=$id" >> $out/exactDeps/$P/configure-flags
        fi
        if ver=$($out/bin/${targetPrefix}ghc-pkg field $P version --simple-output); then
          echo "constraint: $P == $ver" >> $out/exactDeps/$P/cabal.config
          echo "constraint: $P installed" >> $out/exactDeps/$P/cabal.config
        fi
      done

      mkdir -p $out/evalDeps
      for P in $($out/bin/${targetPrefix}ghc-pkg list --simple-output | sed 's/-[0-9][0-9.]*//g'); do
        touch $out/evalDeps/$P
        if id=$($out/bin/${targetPrefix}ghc-pkg field $P id --simple-output); then
          echo "package-id $id" >> $out/evalDeps/$P
        fi
      done
    '';
    ghcForBuilding810
      = if (final.buildPlatform.isAarch64 && final.buildPlatform.isDarwin)
          then final.buildPackages.buildPackages.haskell-nix.bootstrap.compiler.ghc8107
        else if (final.buildPlatform.isAarch64 || final.targetPlatform.isAarch64)
          then final.buildPackages.buildPackages.haskell-nix.compiler.ghc884
        else final.buildPackages.buildPackages.haskell-nix.compiler.ghc865;
    bootPkgs = with final.buildPackages; {
      ghc = final.buildPackages.buildPackages.haskell-nix.bootstrap.compiler."${buildBootstrapper.compilerNixName}";
      alex = final.haskell-nix.bootstrap.packages.alex-unchecked;
      happy = final.haskell-nix.bootstrap.packages.happy-unchecked;
      hscolour = final.haskell-nix.bootstrap.packages.hscolour-unchecked;
    };
    sphinx = with final.buildPackages; (python3Packages.sphinx_1_7_9 or python3Packages.sphinx);
in {
  _dep = (prev._dep or {}) // rec {
    textSrc = final.fetchgit {
      url = "https://github.com/dfordivam/text.git";
      rev = "126174753ea8e5f45df8fcbba609e3f1c453bf27";
      sha256 = "0l7nbln2w77s12fm4ybhi0jsfnxkyiwskfx3b682pfisa6n32rgm";
    };
    ghcjsBaseTextJSStringSrc = (final.fetchgit {
      url = "https://github.com/ghcjs/ghcjs-base.git";
      rev = "85e31beab9beffc3ea91b954b61a5d04e708b8f2";
      sha256 = "sha256-7VYfQS7qFE/itNIv/Nx5B7glL3WkgmmWoIskd8yivd0=";
    }).overrideAttrs (old: {
      outputHash = "sha256-7VYfQS7qFE/itNIv/Nx5B7glL3WkgmmWoIskd8yivd0=";
      postFetch = (old.postFetch or "") + ''
        cd $out
        patch -p1 < ${jsstring-patch}
      '';
    });
    jsstring-patch = (final.fetchurl {
      url = "https://github.com/reflex-frp/reflex-platform/raw/develop/haskell-overlays/ghcjs-text-jsstring-8.10/ghcjs-base-text-jsstring.patch";
      sha256 = "sha256-QmPGCakSWqqg1zXM4MU4pHOk+O+y9ZSt+My+ecl5+nM=";
    });
  };

  obsidianCompilers = {
    jsstring-overrides = [
      { packages.jsaddle.src = (final._dep.source.jsaddle) + "/jsaddle"; }
      ({ pkgs, config, lib, ... }: {
        packages.hashable.patches = [ ../patches/hashable/hashable.patch ];
        packages.aeson = {
          components.library.ghcOptions = [
            "-package ghcjs-base"
            "-package transformers"
          ];
        };
      })
      ({ pkgs, config, lib, ... }: {
        packages.attoparsec = {
          components.sublibs.attoparsec-internal = {
            ghcOptions = [
              "-package ghcjs-base"
            ];
          };
        };
      })
    ];
    ghcjs = builtins.mapAttrs (_: v: v // { useLLVM = false; }) {
      ghcjs8107 = let
        buildGHC = final.buildPackages.haskell-nix.compiler.ghcjs8107;
      in let booted-ghcjs = (final.callPackage (final._dep.source."haskell.nix" + "/compiler/ghcjs/ghcjs.nix") {
          ghcjsSrcJson = (final._dep.source."haskell.nix" + "/compiler/ghcjs/ghcjs810-src.json");
          ghcjsVersion =  "8.10.7"; # Must match the version in the ghcjs.cabal file
          ghc = buildGHC;
          ghcVersion = "8.10.7";
          compiler-nix-name = "ghcjs8107";
        }).overrideAttrs (drv: {
          phases = (drv.phases or []) ++ [ "unpackPhase" "patchPhase" "buildPhase" ];
          postUnpack = ''
            set -x
            (
              echo $sourceRoot
              cd $sourceRoot
              rm -r lib/boot/pkg/text
              # unpackFile ${final._dep.textSrc}
              # chmod +w text-*
              # mv text-* lib/boot/pkg/text
              cp --no-preserve=mode -r "${final._dep.textSrc}" lib/boot/pkg/text
              unpackFile ${final._dep.ghcjsBaseTextJSStringSrc}
              chmod +w ghcjs-base-*
              mv ghcjs-base-* lib/boot/pkg/ghcjs-base
              unpackFile ${final.haskell.packages.ghcjs810.dlist.src}
              chmod +w dlist-*
              mv dlist-* lib/boot/pkg/dlist
              unpackFile ${final.haskell.packages.ghcjs810.vector.src}
              chmod +w vector-*
              mv vector-* lib/boot/pkg/vector
              unpackFile ${final.haskell.packages.ghcjs810.primitive.src}
              chmod +w primitive-*
              mv primitive-* lib/boot/pkg/primitive
              sed -i 's/    - mtl/    - mtl\n    - dlist\n    - primitive\n    - vector\n    - ghcjs-base/' lib/boot/boot.yaml
              cat lib/boot/boot.yaml
            )
            '';
          patches = [
            (final.fetchurl {
              url = "https://github.com/reflex-frp/reflex-platform/raw/develop/haskell-overlays/ghcjs-8.10-fast-weak/fast-weak.patch";
              sha256 = "sha256-ldHC96D/bJxlXmfar/apPj3QZ4P1tnSVNY5ELFvXH/I=";
            })
            ../patches/ghcjs/revert.patch
          ];
        });
      in let targetPrefix = "js-unknown-ghcjs-"; in final.runCommand "${targetPrefix}ghc-8.10.7" {
          nativeBuildInputs = [ final.xorg.lndir ];
            passthru = {
              inherit targetPrefix;
              version = "8.10.7";
              isHaskellNixCompiler = true;
              enableShared = false;
              inherit (booted-ghcjs) configured-src bundled-ghcjs project;
              inherit booted-ghcjs buildGHC;
              extraConfigureFlags = [
                "--ghcjs"
                "--with-ghcjs=${targetPrefix}ghc"
                "--with-ghcjs-pkg=${targetPrefix}ghc-pkg"
                "--with-gcc=${final.buildPackages.emscripten}/bin/emcc"
              ];
            };
          } (''
                mkdir -p $out/bin
                cd $out/bin
                ln -s ${booted-ghcjs}/bin/ghcjs ${targetPrefix}ghc
                ln -s ${booted-ghcjs}/bin/ghcjs-pkg ${targetPrefix}ghc-pkg
                ln -s ${buildGHC}/bin/hsc2hs ${targetPrefix}hsc2hs
                cd ..
                mkdir -p lib/${targetPrefix}ghc-8.10.7
                cd lib
                lndir ${booted-ghcjs}/lib ${targetPrefix}ghc-8.10.7
            '' + installDeps targetPrefix);
    };
    ghc = {
      ghcjs8107 = prev.haskell-nix.compiler.ghc8107;
      ghc8107Splices = final.callPackage (final._dep.source."haskell.nix" + "/compiler/ghc") {
        extra-passthru = {
          buildGHC = final.buildPackages.haskell-nix.compiler.ghc8107;
        };

        bootPkgs = bootPkgs // {
          ghc = ghcForBuilding810;
        };
        inherit sphinx installDeps;

        buildLlvmPackages = final.buildPackages.llvmPackages_12;
        llvmPackages = final.llvmPackages_12;

        src-spec = rec {
          version = "8.10.7";
          url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
          sha256 = "179ws2q0dinl1a39wm9j37xzwm84zfz3c5543vz8v479khigdvp3";
        };
        ghc-patches = [
          ((final._dep.source."haskell.nix") + "/overlays/patches/ghc/Cabal-unbreak-GHCJS.patch")
          (final.fetchurl {
            url = "https://raw.githubusercontent.com/obsidiansystems/splices-load-save.nix/master/patches/ghc-8.10.7/splices.patch";
            sha256 = "sha256-pIMPDpBwL3tYPEbIgTfE1oNgL2KMLp7ovcp6E2KOIVY=";
          })
        ];
      };
    };
  };
}
