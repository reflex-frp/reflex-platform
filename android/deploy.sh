#!/usr/bin/env bash

PATH="$PATH:@java@/bin:@adb@/bin:@coreutils@/bin"

APK="$(echo @out@/*.apk)"

sign=
store_file=
key_alias=
store_password=
key_password=
while [ $# -gt 0 ]; do
  case "$1" in
    --sign)
      sign=1
      shift
    ;;
    --store-file)
      shift
      store_file="$1"
      shift
    ;;
    --store-password)
      shift
      store_password="$1"
      shift
    ;;
    --key-alias)
      shift
      key_alias="$1"
      shift
    ;;
    --key-password)
      shift
      key_password="$1"
      shift
    ;;
    *)
      >&2 echo Unrecognized argument "$1"
      exit 1
    ;;
  esac
done

# Sign at deploy time to prevent private information from leaking to
# the Nix store.
if [ -n "$sign" ]; then
  if [ -z "$store_file" ] || [ -z "$key_alias" ] || [ -z "$store_password" ] || [ -z "$key_password" ]; then
    >&2 echo Please pass arguments for --store-file, --key-alias, --store-password, AND --key-password
    exit 1
  fi

  # Create writable temp file
  signed_apk=$(mktemp --suffix=.apk)
  cp "$APK" "$signed_apk"
  chmod +w "$signed_apk"
  APK="$signed_apk"

  # This should be the equivalent to what Gradle does with
  # ‘signingConfig’. Using
  # https://stackoverflow.com/questions/21457538/how-to-use-jarsigner-for-signing-an-apk/21458940
  # as a reference.

  jarsigner -sigalg SHA1withRSA \
            -digestalg SHA1 \
            -keystore "$store_file" \
            -keypass "$key_password" \
            -storepass "$store_password" \
            "$APK" \
            "$key_alias"

fi

adb install -r "$APK"
