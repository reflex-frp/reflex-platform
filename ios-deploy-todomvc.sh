#!/usr/bin/env bash

function cleanup {
  if [ -n "$tmpdir" -a -d "$tmpdir" ]; then
    echo "Cleaning up tmpdir" >&2
    rm -fR $tmpdir
  fi
  if [ -n "$uuid" ]; then
    echo "Cleaning up simulator" >&2
    xcrun simctl shutdown $uuid 2>/dev/null
    xcrun simctl delete $uuid
  fi
}

trap cleanup EXIT

if [ -z "$1" ]; then
  echo "Usage: ./ios-deploy-todomvc.sh [TEAM_ID]" >&2
  exit 1
fi

tmpdir=$(mktemp -d)
# Find the signer given the ou
signer=`security find-certificate -c "iPhone Developer" -a \
  | grep '^    "alis"<blob>="' \
  | sed 's|    "alis"<blob>="\(.*\)"$|\1|' \
  | while read c; do security find-certificate -c "$c" -p \
  | openssl x509 -subject -noout; done \
  | grep "$1" \
  | sed 's|subject= /UID=[^/]*/CN=\([^/]*\).*|\1|' \
  | head -n 1`

if [ -z "$signer" ]; then
  echo "Error: No iPhone Developer certificate found for team id $1" >&2
  exit 1
fi

nix-build -A ghcIosArm64.reflex-todomvc
mkdir -p $tmpdir/reflex-todomvc.app
cp -r `nix-build -A ghcIosArm64.reflex-todomvc`/reflex-todomvc.app/* $tmpdir/reflex-todomvc.app
sed "s|<team-id/>|$1|" < reflex-todomvc/reflex-todomvc.app.xcent > $tmpdir/reflex-todomvc.app.xcent
cat $tmpdir/reflex-todomvc.app.xcent
/usr/bin/codesign --force --sign "$signer" --entitlements $tmpdir/reflex-todomvc.app.xcent --timestamp=none $tmpdir/reflex-todomvc.app
ios-deploy -b $tmpdir/reflex-todomvc.app

