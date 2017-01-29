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

tmpdir=$(mktemp -d)

nix-build -A ghcIosArm64.reflex-todomvc
mkdir -p $tmpdir/reflex-todomvc.app
cp -r `nix-build -A ghcIosArm64.reflex-todomvc`/reflex-todomvc.app/* $tmpdir/reflex-todomvc.app
/usr/bin/codesign --force --sign "iPhone Developer: Hamish Mackenzie" --entitlements reflex-todomvc/reflex-todomvc.app.xcent --timestamp=none $tmpdir/reflex-todomvc.app
ios-deploy -b $tmpdir/reflex-todomvc.app "$@"

