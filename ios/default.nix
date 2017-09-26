{ nixpkgs
, host
}:

{ #TODO
  bundleName
, #TODO
  bundleIdentifier
, #TODO
  bundleVersionString
, #TODO
  bundleVersion
, #TODO
  exeName
, #TODO
  exePath
, #TODO
  staticSrc
, #TODO
  apsEnv
}:

nixpkgs.runCommand "${exeName}-app" (rec {
  inherit exePath;
  infoPlist = builtins.toFile "Info.plist" ''
    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
    <plist version="1.0">
    <dict>
      <key>CFBundleDevelopmentRegion</key>
      <string>en</string>
      <key>CFBundleExecutable</key>
      <string>${exeName}</string>
      <key>CFBundleIdentifier</key>
      <string>${bundleIdentifier}</string>
      <key>CFBundleInfoDictionaryVersion</key>
      <string>6.0</string>
      <key>CFBundleName</key>
      <string>${bundleName}</string>
      <key>CFBundlePackageType</key>
      <string>APPL</string>
      <key>CFBundleShortVersionString</key>
      <string>${bundleVersionString}</string>
      <key>CFBundleVersion</key>
      <string>${bundleVersion}</string>
      <key>CFBundleSupportedPlatforms</key>
      <array>
        <string>iPhoneOS</string>
      </array>
      <key>LSRequiresIPhoneOS</key>
      <true/>
      <key>NSPhotoLibraryUsageDescription</key>
      <string>Allow access to photo library.</string>
      <key>NSCameraUsageDescription</key>
      <string>Allow access to camera.</string>
      <key>UILaunchStoryboardName</key>
      <string>LaunchScreen</string>
      <key>UIRequiredDeviceCapabilities</key>
      <array>
        <string>arm64</string>
      </array>
      <key>UIDeviceFamily</key>
      <array>
        <integer>1</integer>
        <integer>2</integer>
      </array>
      <key>UISupportedInterfaceOrientations</key>
      <array>
        <string>UIInterfaceOrientationPortrait</string>
        <string>UIInterfaceOrientationLandscapeLeft</string>
        <string>UIInterfaceOrientationLandscapeRight</string>
      </array>
      <key>UISupportedInterfaceOrientations~ipad</key>
      <array>
        <string>UIInterfaceOrientationPortrait</string>
        <string>UIInterfaceOrientationPortraitUpsideDown</string>
        <string>UIInterfaceOrientationLandscapeLeft</string>
        <string>UIInterfaceOrientationLandscapeRight</string>
      </array>
      <key>CFBundleIcons~ipad</key>
      <dict>
        <key>CFBundlePrimaryIcon</key>
        <dict>
          <key>CFBundleIconFiles</key>
          <array>
            <string>Icon-60</string>
            <string>Icon-76</string>
            <string>Icon-83.5</string>
          </array>
        </dict>
      </dict>
      <key>CFBundleIcons</key>
      <dict>
        <key>CFBundlePrimaryIcon</key>
        <dict>
          <key>CFBundleIconFiles</key>
          <array>
            <string>Icon-60</string>
          </array>
        </dict>
      </dict>
      <key>DTSDKName</key>
      <string>iphoneos10.2</string>
      <key>DTXcode</key>
      <string>0821</string>
      <key>DTSDKBuild</key>
      <string>14C89</string>
      <key>BuildMachineOSBuild</key>
      <string>16D32</string>
      <key>DTPlatformName</key>
      <string>iphoneos</string>
      <key>DTCompiler</key>
      <string>com.apple.compilers.llvm.clang.1_0</string>
      <key>MinimumOSVersion</key>
      <string>10.2</string>
      <key>DTXcodeBuild</key>
      <string>8C1002</string>
      <key>DTPlatformVersion</key>
      <string>10.2</string>
      <key>DTPlatformBuild</key>
      <string>14C89</string>
    </dict>
    </plist>
  '';
  resourceRulesPlist = builtins.toFile "ResourceRules.plist" ''
    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
    <plist version="1.0">
    <dict>
      <key>rules</key>
      <dict>
        <key>.*</key>
        <true/>
        <key>Info.plist</key>
        <dict>
          <key>omit</key>
          <true/>
          <key>weight</key>
          <real>10</real>
        </dict>
        <key>ResourceRules.plist</key>
        <dict>
          <key>omit</key>
          <true/>
          <key>weight</key>
          <real>100</real>
        </dict>
      </dict>
    </dict>
    </plist>
  '';
  indexHtml = builtins.toFile "index.html" ''
    <html>
      <head>
      </head>
      <body>
      </body>
    </html>
  '';
  xcent = builtins.toFile "xcent" (''
    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
    <plist version="1.0">
    <dict>
      <key>application-identifier</key>
      <string><team-id/>.${bundleIdentifier}</string>
      <key>com.apple.developer.team-identifier</key>
      <string><team-id/></string>
      <key>get-task-allow</key>
      <true/>
      <key>keychain-access-groups</key>
      <array>
        <string><team-id/>.${bundleIdentifier}</string>
      </array>
      <key>aps-environment</key>
      <string>${apsEnv}</string>
  ''
  + (if host == null then "" else ''
      <key>com.apple.developer.associated-domains</key>
      <array>
        <string>applinks:${host}</string>
        <string>applinks:*.${host}</string>
      </array>
  '')
  + ''
    </dict>
    </plist>
  '');
  deployScript = builtins.toFile "deploy" ''
    #!/usr/bin/env bash
    set -eo pipefail

    if [ -z "$1" -o -z "$2" ]; then
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
      | while read c; do security find-certificate -c "$c" -p \
      | openssl x509 -subject -noout; done \
      | grep "OU=$TEAM_ID/" \
      | sed 's|subject= /UID=[^/]*/CN=\([^/]*\).*|\1|' \
      | head -n 1)

    if [ -z "$signer" ]; then
      echo "Error: No iPhone Developer certificate found for team id $TEAM_ID" >&2
      exit 1
    fi

    mkdir -p $tmpdir
    cp -LR "$(dirname $0)/../${exeName}.app" $tmpdir
    chmod +w "$tmpdir/${exeName}.app"
    mkdir -p "$tmpdir/${exeName}.app/config"
    sed "s|<team-id/>|$TEAM_ID|" < "${xcent}" > $tmpdir/xcent
    /usr/bin/codesign --force --sign "$signer" --entitlements $tmpdir/xcent --timestamp=none "$tmpdir/${exeName}.app"

    "$(nix-build --no-out-link -A nixpkgs.nodePackages.ios-deploy)/bin/ios-deploy" -W -b "$tmpdir/${exeName}.app" "$@"
  '';
  packageScript = builtins.toFile "package" ''
    #!/usr/bin/env bash
    set -eo pipefail

    if [ -z "$1" -o -z "$2" ]; then
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
      | while read c; do security find-certificate -c "$c" -p \
      | openssl x509 -subject -noout; done \
      | grep "OU=$TEAM_ID/" \
      | sed 's|subject= /UID=[^/]*/CN=\([^/]*\).*|\1|' \
      | head -n 1)

    if [ -z "$signer" ]; then
      echo "Error: No iPhone Distribution certificate found for team id $TEAM_ID" >&2
      exit 1
    fi

    mkdir -p $tmpdir
    cp -LR "$(dirname $0)/../${exeName}.app" $tmpdir
    chmod +w "$tmpdir/${exeName}.app"
    chmod +rw "$tmpdir/${exeName}.app/${exeName}"
    strip "$tmpdir/${exeName}.app/${exeName}"
    mkdir -p "$tmpdir/${exeName}.app/config"
    sed "s|<team-id/>|$TEAM_ID|" < "${xcent}" > $tmpdir/xcent
    /usr/bin/codesign --force --sign "$signer" --entitlements $tmpdir/xcent --timestamp=none "$tmpdir/${exeName}.app"

    /usr/bin/xcrun -sdk iphoneos ${./PackageApplication} -v "$tmpdir/${exeName}.app" -o "$IPA_DESTINATION" --sign "$signer" --embed "$EMBEDDED_PROVISIONING_PROFILE"
    /Applications/Xcode.app/Contents/Applications/Application\ Loader.app/Contents/Frameworks/ITunesSoftwareService.framework/Versions/A/Support/altool --validate-app -f "$IPA_DESTINATION" -t ios "$@"
  '';
  runInSim = builtins.toFile "run-in-sim" ''
    #!/usr/bin/env bash

    if [ -z "$1" ]; then
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
    cp -LR "$(dirname $0)/../${exeName}.app" $tmpdir
    chmod +w "$tmpdir/${exeName}.app"
    mkdir -p "$tmpdir/${exeName}.app/config"
    cp "$1" "$tmpdir/${exeName}.app/config/route"
    focus/reflex-platform/run-in-ios-sim "$tmpdir/${exeName}.app"
  '';
}) ''
  set -x
  mkdir -p "$out/${exeName}.app"
  ln -s "$infoPlist" "$out/${exeName}.app/Info.plist"
  ln -s "$resourceRulesPlist" "$out/${exeName}.app/ResourceRules.plist"
  ln -s "$indexHtml" "$out/${exeName}.app/index.html"
  mkdir -p "$out/bin"
  cp --no-preserve=mode "$deployScript" "$out/bin/deploy"
  chmod +x "$out/bin/deploy"
  cp --no-preserve=mode "$packageScript" "$out/bin/package"
  chmod +x "$out/bin/package"
  cp --no-preserve=mode "$runInSim" "$out/bin/run-in-sim"
  chmod +x "$out/bin/run-in-sim"
  ln -s "$exePath/${exeName}" "$out/${exeName}.app/"
  cp -RL "${staticSrc}"/* "$out/${exeName}.app/"
  for icon in "${staticSrc}"/assets/Icon*.png; do
    cp -RL "$icon" "$out/${exeName}.app/"
  done
''
