{ nixpkgs
, nixpkgsCross
, ghcAndroidArm64
, ghcAndroidArmv7a
}:
with nixpkgs.lib.strings;
let androidSOs = mkPackage: nixpkgs.lib.mapAttrs (abiVersion: { myNixpkgs, myHaskellPackages }: {
      inherit (myNixpkgs) libiconv;
      hsApp = mkPackage myHaskellPackages;
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
    intentFilterXml = x: # x :: ["scheme:" "host" ":port" "subdomain_pattern"], see 'androidConfig.deepLinkUris'
      let protocol = removeSuffix ":" (builtins.elemAt x 0);
          host = builtins.elemAt x 1;
          port = removePrefix ":" (builtins.elemAt x 2);
          prefix = builtins.elemAt x 3;
      in ''
      <intent-filter android:autoVerify="true">
        <action android:name="android.intent.action.VIEW" />
        <category android:name="android.intent.category.DEFAULT" />
        <category android:name="android.intent.category.BROWSABLE" />
        <data android:scheme="${protocol}"
              android:host="${prefix + host}"
              ${ if (stringLength port > 0) then ("android:port=\"" + port + "\"") else "" }
              android:pathPrefix="/" />
      </intent-filter>
    '';
in rec {
  #TODO: Provide a better default keystore, or make it unnecessary for debug builds
  #TODO: Allow the user to avoid keystore things completely when doing a debug build
  buildApp =
    { package
    , name
    , packagePrefix
    , icon ? "@drawable/ic_launcher"
    , permissions ? ""
    # URI information that becomes AndroidManifest.xml content for additional intent filters.
    # Expected format: [scheme domain port subdomain_pattern]
    # E.g., ["https:" "obsidian.systems" ":8000" "*."]
    , deepLinkUris ? []
    , key ? null
    , version ? { code = "1"; name = "1.0"; }
    }: nixpkgs.androidenv.buildGradleApp {
    acceptAndroidSdkLicenses = true;
    buildDirectory = "./.";
    # Can be "assembleRelease" or "assembleDebug" (to build release or debug) or "assemble" (to build both)
    gradleTask = "assembleDebug";
    keyAlias = null;
    keyAliasPassword = null;
    keyStore = null;
    keyStorePassword = null;
    mavenDeps = import ./defaults/deps.nix;
    inherit name;
    platformVersions = [ "25" ];
    release = false;
    src = import ./build.nix {
      inherit nixpkgs name packagePrefix;
      appSOs = androidSOs package;
      iconResource = icon;
      assets = nixpkgs.runCommand "android_asset" {} ''
        mkdir "$out"
      '';
      versionName = version.name;
      versionCode = version.code;
      intentFilters = concatStrings (map intentFilterXml deepLinkUris);
      permissions = permissions;
    };
    useExtraSupportLibs = true; #TODO: Should this be enabled by default?
    useGoogleAPIs = true; #TODO: Should this be enabled by default?
    useNDK = true;
  };
}
