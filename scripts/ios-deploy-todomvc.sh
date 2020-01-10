#!/usr/bin/env bash
set -euo pipefail

function cleanup {
  if [ -n "$tmpdir" -a -d "$tmpdir" ]; then
    echo "Cleaning up tmpdir" >&2
    rm -fR $tmpdir
  fi
}

trap cleanup EXIT

if [ -z "$1" ]; then
  echo "Usage: ./ios-deploy-todomvc.sh [TEAM_ID]" >&2
  exit 1
fi

tmpdir=$(mktemp -d)
# Find the signer given the OU
signer=$({ security find-certificate -c 'iPhone Developer' -a; security find-certificate -c 'Apple Development' -a; } \
  | grep '^    "alis"<blob>="' \
  | sed 's|    "alis"<blob>="\(.*\)"$|\1|' \
  | while read c; do security find-certificate -c "$c" -p \
  | openssl x509 -subject -noout; done \
  | grep "OU=$1/" \
  | sed 's|subject= /UID=[^/]*/CN=\([^/]*\).*|\1|' \
  | head -n 1)

if [ -z "$signer" ]; then
  echo "Error: No iPhone Developer certificate found for team id $1" >&2
  exit 1
fi

mkdir -p $tmpdir/reflex-todomvc.app
cp -r `nix-build --no-out-link -A ghcIosAarch64.reflex-todomvc`/reflex-todomvc.app/* $tmpdir/reflex-todomvc.app
sed "s|<team-id/>|$1|" < "$(eval "echo $(nix-instantiate --eval -E '(import ./. {}).ghcIosAarch64.reflex-todomvc.src')")/reflex-todomvc.app.xcent" > $tmpdir/reflex-todomvc.app.xcent
/usr/bin/codesign --force --sign "$signer" --entitlements $tmpdir/reflex-todomvc.app.xcent --timestamp=none $tmpdir/reflex-todomvc.app
"$(nix-build --no-out-link -A nixpkgs.nodePackages.ios-deploy)/bin/ios-deploy" -W -b $tmpdir/reflex-todomvc.app
