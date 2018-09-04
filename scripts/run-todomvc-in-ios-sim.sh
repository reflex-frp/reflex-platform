#!/usr/bin/env bash
set -euo pipefail

function cleanup {
  if [ -n "$uuid" ]; then
    echo "Cleaning up simulator" >&2
    xcrun simctl shutdown $uuid 2>/dev/null
    xcrun simctl delete $uuid
  fi
}

trap cleanup EXIT

nix-build -A ghcIosSimulator64.reflex-todomvc

uuid=$(xcrun simctl create reflex-todomvc com.apple.CoreSimulator.SimDeviceType.iPhone-7 com.apple.CoreSimulator.SimRuntime.iOS-10-2)
open -a Simulator --args -CurrentDeviceUDID $uuid
xcrun simctl install $uuid `nix-build -A ghcIosSimulator64.reflex-todomvc`/reflex-todomvc.app
xcrun simctl launch --console $uuid reflex-todomvc
