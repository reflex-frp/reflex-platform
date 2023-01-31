{ useTextJSString ? false, useFastWeak ? false }: final: prev: let
 haskell-nix = final.nix-thunk.thunkSource ../../dep/haskell.nix;
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
  obsidianCompilers = {
    ghcjs = builtins.mapAttrs (_: v: v // { useLLVM = false; }) {
      ghc8107Splices = let
        buildGHC = final.buildPackages.haskell-nix.compiler.ghc8107;
      in let booted-ghcjs = final.callPackage (haskell-nix + "/compiler/ghcjs/ghcjs.nix") {
          ghcjsSrcJson = (haskell-nix + "/compiler/ghcjs/ghcjs810-src.json");
          ghcjsVersion =  "8.10.7"; # Must match the version in the ghcjs.cabal file
          ghc = buildGHC;
          ghcVersion = "8.10.7";
          compiler-nix-name = "ghc8107";
      }; in let targetPrefix = "js-unknown-ghcjs-"; in final.runCommand "${targetPrefix}ghc-8.10.7" {
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
      ghc8107Splices = final.callPackage (haskell-nix + "/compiler/ghc") {
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
          (final.fetchurl {
            url = "https://raw.githubusercontent.com/obsidiansystems/splices-load-save.nix/master/patches/ghc-8.10.7/splices.patch";
            sha256 = "sha256-pIMPDpBwL3tYPEbIgTfE1oNgL2KMLp7ovcp6E2KOIVY=";
          })
        ];
      };
    };
  };
}
