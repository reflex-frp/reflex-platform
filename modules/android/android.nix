env: with env; let
  androidenv = pkgs.androidenv;
  buildGradleApp = import ./build-gradle-app.nix {
      inherit (pkgs) stdenv lib jdk gnumake gawk file runCommand
                     which gradle fetchurl buildEnv;
      inherit androidenv;
  };

  unpackTars = tar: pkgs.runCommandNoCC "unpack-tar" { inherit tar; } ''
    mkdir -p $out
    tar -zxf $tar --directory $out
  '';
in {
  buildApp = args: with args; buildGradleApp {
    inherit acceptAndroidSdkLicenses mavenDeps;
    buildDirectory = "./.";
    inherit gradleTask;
    keyAlias = releaseKey.keyAlias or null;
    keyAliasPassword = releaseKey.keyPassword or null;
    keyStore = releaseKey.storeFile or null;
    keyStorePassword = releaseKey.storePassword or null;
    name = applicationId;
    platformVersions = [ "30" ];
    release = false;
    src =
      let 
       # splitApplicationId = splitString "." applicationId;
          appSOs = {
            "arm64-v8a" = {
              hsApp = pkg-set.config.hsPkgs.reflex-todomvc.library;
              sharedLibs = runtimeSharedLibs ++ [ "${buildPackages.pkgsCross.libffi}" ];
            };
          };
          abiVersions = attrNames appSOs;
      in pkgs.runCommand "android-app" {
        buildGradle = builtins.toFile "build.gradle" (import ./build.gradle.nix {
          inherit applicationId version additionalDependencies releaseKey universalApk;
          googleServicesClasspath = optionalString (googleServicesJson != null)
            "classpath 'com.google.gms:google-services:4.3.3'";
          googleServicesPlugin = optionalString (googleServicesJson != null)
            "apply plugin: 'com.google.gms.google-services'";
        });
        androidManifestXml = builtins.toFile "AndroidManifest.xml" (import ./AndroidManifest.xml.nix {
          inherit applicationId version iconPath intentFilters services permissions activityAttributes usesCleartextTraffic;
        });
        stringsXml = builtins.toFile "strings.xml" (import ./strings.xml.nix {
          inherit displayName;
        });
        applicationMk = builtins.toFile "Application.mk" (import ./Application.mk.nix {
          inherit pkgs abiVersions;
        });
        javaSrc = pkgs.buildEnv {
          name = applicationId + "-java";
          paths = javaSources 
          ((unpackTars pkg-set.config.hsPkgs.reflex-dom.src) + "/java") 
          ((pkgs.fetchFromGitHub {
            owner = "obsidian.systems";
            repo = "android-activity";
            rev = "a51bf130b04af92645c040df065c54161e99a335";
            sha256 = "";
          }) + "/java");

            # Sets up the main Activity using [android-activity](https://hackage.haskell.org/package/android-activity)
            #(ghcAndroidAarch64.android-activity.src + "/java") #TODO: Use output, not src
            # Sets up the main webview using [reflex-dom](https://github.com/reflex-frp/reflex-dom/blob/develop/reflex-dom/java/org/reflexfrp/reflexdom/MainWidget.java)
            #(ghcAndroidAarch64.reflex-dom.src + "/java");
        };
        src = ./src;
        nativeBuildInputs = [ pkgs.rsync ];
        unpackPhase = "";
      } (''
          cp -r --no-preserve=mode "$src" "$out"
          mkdir -p "$out/src/main"
          cp -r --no-preserve=mode "$javaSrc" "$out/src/main/java"
          ln -s "$buildGradle" "$out/build.gradle"
          ln -s "$androidManifestXml" "$out/AndroidManifest.xml"
          mkdir -p "$out/res/values"
          ln -s "$stringsXml" "$out/res/values/strings.xml"
          mkdir -p "$out/jni"
          ln -s "$applicationMk" "$out/jni/Application.mk"
          ${optionalString (googleServicesJson != null) ''cp '${googleServicesJson}' "$out/google-services.json"''}
        '' + concatStrings (builtins.map (arch:
          let
            inherit (appSOs.${arch}) hsApp sharedLibs;
            sharedLibsCmd = concatStrings (map (libPath: ''
              local lib="${libPath}"
              if [ ! -f "$lib" ] ; then
                >&2 echo 'Error: library $lib not found'
                exit 1
              fi
              cp --no-preserve=mode "$lib" "$ARCH_LIB"
              '') sharedLibs);
          in ''
            {
              ARCH_LIB=$out/lib/${arch}
              mkdir -p $ARCH_LIB

              local exe="${hsApp}/bin/lib${executableName}.so"
              if [ ! -f "$exe" ] ; then
                >&2 echo 'Error: executable "${executableName}" not found'
                exit 1
              fi
              cp --no-preserve=mode "$exe" "$ARCH_LIB/libHaskellActivity.so"

              '' + sharedLibsCmd + ''
            }
        '') abiVersions) + ''
          rsync -r --chmod=+w "${assets}"/ "$out/assets/"
          rsync -r --chmod=+w "${resources}"/ "$out/res/"
          [ -d "$out/assets" ]
          [ -d "$out/res" ]
        '');
    useGooglePlayServices = true; # TODO: Should this be enabled by default?
    useGoogleAPIs = true; #TODO: Should this be enabled by default?

    # We use the NDK build process
    useNDK = true;
  };
  inherit fixupCabal;
}
