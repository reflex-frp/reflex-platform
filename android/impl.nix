env: with env;
let overrideAndroidCabal = package: overrideCabal package (drv: {
      preConfigure = (drv.preConfigure or "") + ''
        sed -i 's%^executable *\(.*\)$%executable lib\1.so\n  cc-options: -shared -fPIC\n  ld-options: -shared -Wl,--gc-sections,--version-script=${./haskellActivity.version},-u,Java_systems_obsidian_HaskellActivity_haskellStartMain,-u,hs_main\n  ghc-options: -shared -fPIC -threaded -no-hs-main -lHSrts_thr -lCffi -lm -llog%i' *.cabal
      '';
    });
    #TODO: Keep the signing key for dev mode more consistent, e.g. in ~/.config/reflex-platform, so that the app can be reinstalled in-place
    addDeployScript = src: nixpkgs.runCommand "android-app" {
      inherit src;
      buildCommand = ''
        mkdir -p "$out/bin"
        cp -r "$src"/* "$out"
        cat >"$out/bin/deploy" <<EOF
          $(which adb) install -r "$(echo $out/*.apk)"
        EOF
        chmod +x "$out/bin/deploy"
      '';
      buildInputs = with nixpkgs; [
        androidsdk
        which
      ];
    } "";
    inherit (nixpkgs.lib) splitString escapeShellArg mapAttrs attrNames concatStrings optionalString;
in {
  buildApp = args: with args; addDeployScript (nixpkgs.androidenv.buildGradleApp {
    acceptAndroidSdkLicenses = true;
    buildDirectory = "./.";
    # Can be "assembleRelease" or "assembleDebug" (to build release or debug) or "assemble" (to build both)
    gradleTask = if releaseKey == null
      then "assembleDebug"
      else "assembleRelease";
    keyAlias = releaseKey.keyAlias or null;
    keyAliasPassword = releaseKey.keyPassword or null;
    keyStore = releaseKey.storeFile or null;
    keyStorePassword = releaseKey.storePassword or null;
    mavenDeps = import ./defaults/deps.nix;
    name = applicationId;
    platformVersions = [ "25" ];
    release = false;
    src =
      let splitApplicationId = splitString "." applicationId;
          appSOs = mapAttrs (abiVersion: { myNixpkgs, myHaskellPackages }: {
            inherit (myNixpkgs) libiconv;
            hsApp = overrideAndroidCabal (package myHaskellPackages);
          }) {
            "arm64-v8a" = {
              myNixpkgs = nixpkgsCross.android.arm64Impure;
              myHaskellPackages = ghcAndroidArm64;
            };
            "armeabi-v7a" = {
              myNixpkgs = nixpkgsCross.android.armv7aImpure;
              myHaskellPackages = ghcAndroidArmv7a;
            };
          };
          abiVersions = attrNames appSOs;
      in nixpkgs.runCommand "android-app" {
        buildGradle = builtins.toFile "build.gradle" (import ./build.gradle.nix {
          inherit applicationId version additionalDependencies releaseKey universalApk;
          googleServicesClasspath = optionalString (googleServicesJson != null)
            "classpath 'com.google.gms:google-services:3.0.0'";
          googleServicesPlugin = optionalString (googleServicesJson != null)
            "apply plugin: 'com.google.gms.google-services'";
        });
        androidManifestXml = builtins.toFile "AndroidManifest.xml" (import ./AndroidManifest.xml.nix {
          inherit applicationId version iconPath intentFilters services permissions activityAttributes;
        });
        stringsXml = builtins.toFile "strings.xml" (import ./strings.xml.nix {
          inherit displayName;
        });
        applicationMk = builtins.toFile "Application.mk" (import ./Application.mk.nix {
          inherit nixpkgs abiVersions;
        });
        javaSrc = nixpkgs.buildEnv {
          name = applicationId + "-java";
          paths = [
            (ghcAndroidArm64.android-activity.src + "/java") #TODO: Use output, not src
            (ghcAndroidArm64.reflex-dom.src + "/java")
          ];
        };
        src = ./src;
        nativeBuildInputs = [ nixpkgs.rsync ];
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

        '' + concatStrings (builtins.map (arch:
          let inherit (appSOs.${arch}) libiconv hsApp;
          in ''
            {
              ARCH_LIB=$out/lib/${arch}
              mkdir -p $ARCH_LIB

              # Move libiconv (per arch) to the correct place
              cp --no-preserve=mode "${libiconv}/lib/libiconv.so" "$ARCH_LIB"
              cp --no-preserve=mode "${libiconv}/lib/libcharset.so" "$ARCH_LIB"

              local exe="${hsApp}/bin/lib${executableName}.so"
              if [ ! -f "$exe" ] ; then
                >&2 echo 'Error: executable "${executableName}" not found'
                exit 1
              fi
              cp --no-preserve=mode "$exe" "$ARCH_LIB/libHaskellActivity.so"
            }
        '') abiVersions) + ''
          rsync -r --chmod=+w "${assets}"/ "$out/assets/"
          rsync -r --chmod=+w "${resources}"/ "$out/res/"
          [ -d "$out/assets" ]
          [ -d "$out/res" ]
        '');
    useExtraSupportLibs = true; #TODO: Should this be enabled by default?
    useGoogleAPIs = true; #TODO: Should this be enabled by default?

    # We use the NDK build process
    useNDK = true;
  });

  intentFilterXml = args: with args; ''
    <intent-filter android:autoVerify="true">
      <action android:name="android.intent.action.VIEW" />
      <category android:name="android.intent.category.DEFAULT" />
      <category android:name="android.intent.category.BROWSABLE" />
      <data android:scheme="${scheme}"
            android:host="${host}"
            ${ optionalString (port != null) ''android:port="${toString port}"'' }
            android:pathPrefix="${pathPrefix}" />
    </intent-filter>
  '';
}
