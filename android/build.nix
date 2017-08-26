# Build the source, to be compiled with gradle
{ name
, packagePrefix
, nixpkgs
, appSOs # a map from arches to sos
, assets
, res ? ./res
, versionCode ? "1"
, versionName ? "1.0" # Visible to end users
, intentFilters ? "" # Manifest XML for additional intent filters
, permissions ? "" # Manifest XML for additional permissions
, googleServicesJson ? null
, additionalDependencies ? ""
, iconResource ? "@mipmap/ic_launcher"
, services ? "" # Manifest XML for additional services
, includeFirebase ? false
, releaseKey ? null
}:

let inherit (nixpkgs) lib runCommand;
    inherit (lib) escapeShellArg;
    appName = name;
    packageName = packagePrefix + "." + name;
    packageSrcDir = "src/main/java/" + builtins.replaceStrings ["."] ["/"] packageName;
    androidSdk = nixpkgs.androidenv.androidsdk_6_0_extras;
    abiVersions = lib.attrNames appSOs;
    stuff = lib.attrValues appSOs;
    libiconvs = builtins.toString (builtins.map (s: s.libiconv) stuff);
    hsApps = builtins.toString (builtins.map (s: s.hsApp) stuff);
in runCommand "android-app" {
  inherit androidSdk; # frontend;
  buildGradle = builtins.toFile "build.gradle" (import ./build.gradle.nix {
    applicationId = packageName; #TODO: Should these be the same?
    inherit versionCode versionName additionalDependencies releaseKey;
    googleServicesClasspath = if googleServicesJson != null then "classpath 'com.google.gms:google-services:3.0.0'" else "";
    googleServicesPlugin = if googleServicesJson != null then "apply plugin: 'com.google.gms.google-services'" else "";
  });
  androidManifest = builtins.toFile "AndroidManifest.xml" (import ./AndroidManifest.xml.nix {
    inherit packageName versionCode versionName iconResource intentFilters services permissions;
  });
  src = ./src;
  nativeBuildInputs = [ nixpkgs.rsync ];
  unpackPhase = "";
} (''
    set -x

    mkdir $out

    # copy build files and do substitutions
    cp $src/project.properties $out
    cp $src/gradle.properties $out

    ln -s "$buildGradle" "$out/build.gradle"

    ${lib.optionalString (googleServicesJson != null) "cp ${googleServicesJson} $out/google-services.json"}

    cp $src/local.properties $out
    substituteInPlace $out/local.properties \
      --subst-var-by SDKDIR "${androidSdk}/libexec"

    cp $src/build.xml $out
    substituteInPlace $out/build.xml \
      --subst-var-by PROJECTNAME "${appName}"

    cp $src/custom_rules.xml $out
    substituteInPlace $out/custom_rules.xml \
      --subst-var-by PROJECTNAME "${appName}"

    cp $src/ant.properties $out

    ln -s "$androidManifest" "$out/AndroidManifest.xml"

    # copy the template project, and then put the src in the right place
    mkdir -p "$out/${packageSrcDir}"
    cp -r --no-preserve=mode "$src/src/." "$out/${packageSrcDir}"

    ${if includeFirebase then "" else ''
      rm $out/${packageSrcDir}/LocalFirebaseInstanceIDService.java
      rm $out/${packageSrcDir}/LocalFirebaseMessagingService.java
    ''}
    sed -i 's|package systems.obsidian.focus;|package '"${packageName}"\;'|' "$out/${packageSrcDir}/"*".java"

    substituteInPlace "$out/${packageSrcDir}/MainActivity.java" \
      --subst-var-by APPNAME "${appName}"

    cp -r --no-preserve=mode "$src/assets" $out

    cp -r --no-preserve=mode "$src/res" $out
    substituteInPlace "$out/res/values/strings.xml" \
      --subst-var-by APPDISPLAYNAME "${appName}"

    cp -r --no-preserve=mode "$src/jni" $out

    substituteInPlace $out/jni/Application.mk \
      --subst-var-by ABI_VERS "${lib.concatStringsSep " " abiVersions}"

  '' + lib.concatStrings (builtins.map (arch:
    let inherit (appSOs.${arch}) libiconv hsApp;
    in ''
      {
        ARCH_LIB=$out/lib/${arch}
        mkdir -p $ARCH_LIB

        # Move libiconv (per arch) to the correct place
        cp --no-preserve=mode "${libiconv}/lib/libiconv.so"   $ARCH_LIB
        cp --no-preserve=mode "${libiconv}/lib/libcharset.so" $ARCH_LIB

        echo "${hsApp}"
        # Point to HS application (per arch) shared object
        APP_LIB_PATH=$(echo "${hsApp}"/bin/*.so)
        APP_LIB_NAME=$(basename "$APP_LIB_PATH")
        cp --no-preserve=mode "$APP_LIB_PATH" "$ARCH_LIB/$APP_LIB_NAME"

        substituteInPlace $out/jni/Android.mk \
          --subst-var-by APP_LIB_NAME "$APP_LIB_NAME"
      }
    '') abiVersions) + ''
    substituteInPlace $out/jni/Android.mk \
      --subst-var-by ICONV_PATH "" \
      --subst-var-by CHARSET_PATH "" \
      --subst-var-by APP_PATH ""

    rsync -r --chmod=+w "${assets}"/ "$out/assets/"
    rsync -r --chmod=+w "${res}"/ "$out/res/"
    [ -d "$out/assets" ]
    [ -d "$out/res" ]
  '')
