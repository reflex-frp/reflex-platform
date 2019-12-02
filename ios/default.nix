{ nixpkgs, ghc, withSimulator ? false }:

{ #TODO
  bundleName

, #TODO
  bundleIdentifier

, #TODO
  bundleVersionString ? "1"

, #TODO
  bundleVersion ? "1"

, #TODO
  executableName

, #TODO
  package

, #TODO
  staticSrc ? ./static

, # Information for push notifications. Is either `"production"` or
  # `"development"`, if not null.
  #
  # Requires the push notification application service to be enabled for this
  # App ID in your Apple developer account.
  apsEnv ? null

, # URL patterns for which to handle links. E.g. `[ "*.mywebsite.com" ]`.
  #
  # Requires the associated domains application service to be enabled for this
  # App ID in your Apple developer account.
  hosts ? []

# Function taking set of plist keys-value pairs and returns a new set with changes applied.
#
# For example: (super: super // { AnotherKey: "value"; })
, overrideInfoPlist ? (super: super)

# REMOVED
, extraInfoPlistContent ? null
}:
let
  defaultInfoPlist = {
    CFBundleDevelopmentRegion = "en";
    CFBundleExecutable = executableName;
    CFBundleIdentifier = bundleIdentifier;
    CFBundleInfoDictionaryVersion = "6.0";
    CFBundleName = bundleName;
    CFBundlePackageType = "APPL";
    CFBundleShortVersionString = bundleVersionString;
    CFBundleVersion = bundleVersion;
    CFBundleSupportedPlatforms = [ "iPhoneOS" ];
    LSRequiresIPhoneOS = true;
    UILaunchStoryboardName = "LaunchScreen";
    UIRequiredDeviceCapabilities = [ "arm64" ];
    UIDeviceFamily = [ 1 2 ];
    UISupportedInterfaceOrientations = [
      "UIInterfaceOrientationPortrait"
      "UIInterfaceOrientationLandscapeLeft"
      "UIInterfaceOrientationLandscapeRight"
    ];
    ${"UISupportedInterfaceOrientations~ipad"} = [
      "UIInterfaceOrientationPortrait"
      "UIInterfaceOrientationPortraitUpsideDown"
      "UIInterfaceOrientationLandscapeLeft"
      "UIInterfaceOrientationLandscapeRight"
    ];
    ${"CFBundleIcons~ipad"} = {
      CFBundlePrimaryIcon = {
        CFBundleIconFiles = [
          "Icon-60"
          "Icon-76"
          "Icon-83.5"
        ];
      };
    };
    CFBundleIcons = {
      CFBundlePrimaryIcon = {
        CFBundleIconFiles = [
          "Icon-60"
        ];
      };
    };
    DTSDKName = "iphoneos12.4";
    DTXcode = "0821";
    DTSDKBuild = "16G73";
    BuildMachineOSBuild = "16D32";
    DTPlatformName = "iphoneos";
    DTCompiler = "com.apple.compilers.llvm.clang.1_0";
    MinimumOSVersion = "10.2";
    DTXcodeBuild = "10G8";
    DTPlatformVersion = "12.4";
    DTPlatformBuild = "14C89";
    NSPhotoLibraryUsageDescription = "Allow access to photo library.";
    NSCameraUsageDescription = "Allow access to camera.";
  };

  infoPlistData = if extraInfoPlistContent == null
    then overrideInfoPlist defaultInfoPlist
    else abort ''
      `extraInfoPlistContent` has been removed. Instead use `overrideInfoPlist` to provide an override function that modifies the default info.plist data as a nix attrset. For example: `(x: x // {NSCameraUsageDescription = "We need your camera.";})`
    '';
in
nixpkgs.runCommand "${executableName}-app" (rec {
  exePath = package ghc;
  infoPlist = builtins.toFile "Info.plist" (nixpkgs.lib.generators.toPlist {} infoPlistData);
  resourceRulesPlist = builtins.toFile "ResourceRules.plist" (nixpkgs.lib.generators.toPlist {} {
    rules = {
      ".*" = true;
      "Info.plist" = {
        omit = true;
        weight = 10;
      };
      "ResourceRules.plist" = {
        omit = true;
        weight = 100;
      };
    };
  });
  indexHtml = builtins.toFile "index.html" ''
    <html>
      <head>
      </head>
      <body>
      </body>
    </html>
  '';
  xcent = builtins.toFile "xcent" (nixpkgs.lib.generators.toPlist {} {
    application-identifier = "<team-id/>.${bundleIdentifier}";
    "com.apple.developer.team-identifier" = "<team-id/>";
    get-task-allow = true;
    keychain-access-groups = [ "<team-id/>.${bundleIdentifier}" ];
    aps-environment = apsEnv;
    "com.apple.developer.associated-domains" =
      if hosts == [] then null else map (host: "applinks:${host}") hosts;
  });
  deployScript = nixpkgs.writeText "deploy" ''
    #!/usr/bin/env bash
    set -eo pipefail

    if (( "$#" < 1 )); then
      echo "Usage: $0 [TEAM_ID]" >&2
      exit 1
    fi

    TEAM_ID=$1
    shift

    set -euo pipefail

    function cleanup {
      if [ -n "$tmpdir" -a -d "$tmpdir" ]; then
        echo "Cleaning up tmpdir" >&2
        chmod -R +w $tmpdir
        rm -fR $tmpdir
      fi
    }

    trap cleanup EXIT

    tmpdir=$(mktemp -d)
    # Find the signer given the OU
    signer=$(security find-certificate -c "iPhone Developer" -a \
      | grep '^    "alis"<blob>="' \
      | sed 's|    "alis"<blob>="\(.*\)"$|\1|' \
      | while read c; do \
          security find-certificate -c "$c" -p \
            | openssl x509 -subject -noout; \
        done \
      | grep "OU=$TEAM_ID/" \
      | sed 's|subject= /UID=[^/]*/CN=\([^/]*\).*|\1|' \
      | head -n 1)

    if [ -z "$signer" ]; then
      echo "Error: No iPhone Developer certificate found for team id $TEAM_ID" >&2
      exit 1
    fi

    mkdir -p $tmpdir
    cp -LR "$(dirname $0)/../${executableName}.app" $tmpdir
    chmod +w "$tmpdir/${executableName}.app"
    chmod +w "$tmpdir/${executableName}.app/${executableName}"
    mkdir -p "$tmpdir/${executableName}.app/config"
    sed "s|<team-id/>|$TEAM_ID|" < "${xcent}" > $tmpdir/xcent
    /usr/bin/codesign --force --sign "$signer" --entitlements $tmpdir/xcent --timestamp=none "$tmpdir/${executableName}.app"

    ${nixpkgs.nodePackages.ios-deploy}/bin/ios-deploy -W -b "$tmpdir/${executableName}.app" "$@"
  '';
  packageScript = nixpkgs.writeText "package" ''
    #!/usr/bin/env bash
    set -eo pipefail

    if (( "$#" != 3 )); then
      echo "Usage: $0 [TEAM_ID] [IPA_DESTINATION] [EMBEDDED_PROVISIONING_PROFILE]" >&2
      exit 1
    fi

    TEAM_ID=$1
    shift
    IPA_DESTINATION=$1
    shift
    EMBEDDED_PROVISIONING_PROFILE=$1
    shift

    set -euo pipefail

    function cleanup {
      if [ -n "$tmpdir" -a -d "$tmpdir" ]; then
        echo "Cleaning up tmpdir" >&2
        chmod -R +w $tmpdir
        rm -fR $tmpdir
      fi
    }

    trap cleanup EXIT

    tmpdir=$(mktemp -d)
    # Find the signer given the OU
    signer=$(security find-certificate -c "iPhone Distribution" -a \
      | grep '^    "alis"<blob>="' \
      | sed 's|    "alis"<blob>="\(.*\)"$|\1|' \
      | while read c; do \
          security find-certificate -c "$c" -p \
            | openssl x509 -subject -noout; \
        done \
      | grep "OU=$TEAM_ID/" \
      | sed 's|subject= /UID=[^/]*/CN=\([^/]*\).*|\1|' \
      | head -n 1)

    if [ -z "$signer" ]; then
      echo "Error: No iPhone Distribution certificate found for team id $TEAM_ID" >&2
      exit 1
    fi

    mkdir -p $tmpdir
    cp -LR "$(dirname $0)/../${executableName}.app" $tmpdir
    chmod +w "$tmpdir/${executableName}.app"
    chmod +rw "$tmpdir/${executableName}.app/${executableName}"
    strip "$tmpdir/${executableName}.app/${executableName}"
    mkdir -p "$tmpdir/${executableName}.app/config"
    sed "s|<team-id/>|$TEAM_ID|" < "${xcent}" > $tmpdir/xcent
    /usr/bin/codesign --force --sign "$signer" --entitlements $tmpdir/xcent --timestamp=none "$tmpdir/${executableName}.app"

    /usr/bin/xcrun -sdk iphoneos ${./PackageApplication} -v "$tmpdir/${executableName}.app" -o "$IPA_DESTINATION" --sign "$signer" --embed "$EMBEDDED_PROVISIONING_PROFILE"
    /Applications/Xcode.app/Contents/Applications/Application\ Loader.app/Contents/Frameworks/ITunesSoftwareService.framework/Versions/A/Support/altool --validate-app -f "$IPA_DESTINATION" -t ios "$@"
  '';
  runInSim = builtins.toFile "run-in-sim" ''
    #!/usr/bin/env bash

    if (( "$#" != 0 )); then
      echo "Usage: $0" >&2
      exit 1
    fi

    set -euo pipefail

    function cleanup {
      if [ -n "$tmpdir" -a -d "$tmpdir" ]; then
        echo "Cleaning up tmpdir" >&2
        chmod -R +w $tmpdir
        rm -fR $tmpdir
      fi
    }

    trap cleanup EXIT

    tmpdir=$(mktemp -d)

    mkdir -p $tmpdir
    cp -LR "$(dirname $0)/../${executableName}.app" $tmpdir
    chmod +w "$tmpdir/${executableName}.app"
    mkdir -p "$tmpdir/${executableName}.app/config"
    ${../scripts/run-in-ios-sim} "$(dirname $0)/../${executableName}.app"
  '';
}) (''
  set -x
  mkdir -p "$out/${executableName}.app"
  ln -s "$infoPlist" "$out/${executableName}.app/Info.plist"
  ln -s "$resourceRulesPlist" "$out/${executableName}.app/ResourceRules.plist"
  ln -s "$indexHtml" "$out/${executableName}.app/index.html"
  mkdir -p "$out/bin"
  cp --no-preserve=mode "$deployScript" "$out/bin/deploy"
  chmod +x "$out/bin/deploy"
  cp --no-preserve=mode "$packageScript" "$out/bin/package"
  chmod +x "$out/bin/package"
'' + nixpkgs.lib.optionalString withSimulator ''
  cp --no-preserve=mode "$runInSim" "$out/bin/run-in-sim"
  chmod +x "$out/bin/run-in-sim"
'' + ''
  ln -s "$exePath/bin/${executableName}" "$out/${executableName}.app/"
  cp -RL "${staticSrc}"/* "$out/${executableName}.app/"
  for icon in "${staticSrc}"/assets/Icon*.png; do
    cp -RL "$icon" "$out/${executableName}.app/"
  done
  set +x
'')
