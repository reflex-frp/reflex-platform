env@{
  nixpkgs
, nixpkgsCross
, ghcAndroidArm64
, ghcAndroidArmv7a
, overrideCabal
}:
with nixpkgs.lib.strings;
let impl = import ./impl.nix env;
in rec {
  # URI information that becomes AndroidManifest.xml content for additional intent filters.
  intentFilterXml = {
      scheme
      # URL scheme
      # E.g.: "https"

    , host

    , port ? null
      # Port number or null
      # E.g.: 8000

    , pathPrefix ? ""
    }: impl.intentFilterXml {
      inherit scheme
              host
              port
              pathPrefix;
    };

  defaultResources = ./res;
  defaultAssets = ./assets;
  defaultIconPath = "@drawable/ic_launcher";

  buildIcons = nixpkgs.callPackage ./buildIcons.nix {};

  buildApp = {
      package
      # A function from haskellPackages to the package we'd like to turn into
      # an APK
      # E.g.: (p: p.hello)

    , executableName
      # The name of the executable in the Cabal file that will become the main
      # Activity in the Android package
      # E.g.: "hello"

    , applicationId
      # The [Application ID](https://developer.android.com/studio/build/application-id.html)
      # for your Android package
      # E.g.: "com.example.myapp"

    , displayName
      # The app name that will be displayed to the user
      # E.g.: "Hello, world!"

    , version ? {
        code = "1";
        # Must be a monotonically increasing number; defines what it means to "upgrade" the app

        name = "1.0";
        # The version that is displayed to the end user
      }

    , releaseKey ? null
      #TODO: Factor out signing into a separate step
      # To create a release build, set this to a value like:
      # { storeFile = ./path/to/keystore;
      #   storePassword = "password";
      #   keyAlias = "myKey";
      #   keyPassword = "password";
      # }

    , resources ? defaultResources

    , assets ? defaultAssets

    , iconPath ? defaultIconPath

    , activityAttributes ? ""
      # Additional activity attributes like: android:launchMode="singleInstance"

    , permissions ? ""
      # Manifest XML for additional permissions

    , services ? ""

    , intentFilters ? ""
      # Manifest XML for additional intent filters
      # E.g.: concatStrings (map intentFilterXml deepLinkUris);

    , googleServicesJson ? null

    , additionalDependencies ? ""

    , universalApk ? true
      # Set this to false to build one APK per target platform.  This will
      # automatically transform the version code to 1000 * versionCode + offset
      # where "offset" is a per-platform constant.
    }:
    assert builtins.match "^([A-Za-z][A-Za-z0-9_]*\\.)*[A-Za-z][A-Za-z0-9_]*$" applicationId != null;
    impl.buildApp {
      inherit package
              executableName
              applicationId
              displayName
              version
              releaseKey
              resources
              assets
              iconPath
              activityAttributes
              permissions
              services
              intentFilters
              googleServicesJson
              additionalDependencies
              universalApk;
    };
}
