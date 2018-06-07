#!/bin/sh

# This script installs the Nix package manager on your system by
# downloading a binary distribution and running its installer script
# (which in turn creates and populates /nix).

{ # Prevent execution if this script was only partially downloaded
oops() {
    echo "$0:" "$@" >&2
    exit 1
}

tmpDir="$(mktemp -d -t nix-binary-tarball-unpack.XXXXXXXXXX || \
          oops "Can\'t create temporary directory for downloading the Nix binary tarball")"
cleanup() {
    rm -rf "$tmpDir"
}
trap cleanup EXIT INT QUIT TERM

require_util() {
    type "$1" > /dev/null 2>&1 || which "$1" > /dev/null 2>&1 ||
        oops "you do not have '$1' installed, which I need to $2"
}

case "$(uname -s).$(uname -m)" in
    Linux.x86_64) system=x86_64-linux; hash=d7e3cba2103c051b17d2a49dc6389f96ebe46dca72dac8473cfb87af8b67ef03;;
    Linux.i?86) system=i686-linux; hash=c9f5c41bea715b72eeab83dd70d5cb42c84469a4b419f918df39ebb8597ffec1;;
    Linux.aarch64) system=aarch64-linux; hash=6b60342fb430a2842b300a72a9cf179dace569713fb27520513c9fcdf94ba0c4;;
    Darwin.x86_64) system=x86_64-darwin; hash=b3e1a7316053fda14b4c69142c9f432aab2742e3a5febaed805a316450d6360d;;
    *) oops "sorry, there is no binary distribution of Nix for your platform";;
esac

url="https://nixos.org/releases/nix/nix-1.11.16/nix-1.11.16-$system.tar.bz2"

tarball="$tmpDir/$(basename "$tmpDir/nix-1.11.16-$system.tar.bz2")"

require_util curl "download the binary tarball"
require_util bzcat "decompress the binary tarball"
require_util tar "unpack the binary tarball"

echo "downloading Nix 1.11.16 binary tarball for $system from '$url' to '$tmpDir'..."
curl -L "$url" -o "$tarball" || oops "failed to download '$url'"

if type sha256sum > /dev/null 2>&1; then
    hash2="$(sha256sum -b "$tarball" | cut -c1-64)"
elif type shasum > /dev/null 2>&1; then
    hash2="$(shasum -a 256 -b "$tarball" | cut -c1-64)"
elif type openssl > /dev/null 2>&1; then
    hash2="$(openssl dgst -r -sha256 "$tarball" | cut -c1-64)"
else
    oops "cannot verify the SHA-256 hash of '$url'; you need one of 'shasum', 'sha256sum', or 'openssl'"
fi

if [ "$hash" != "$hash2" ]; then
    oops "SHA-256 hash mismatch in '$url'; expected $hash, got $hash2"
fi

unpack=$tmpDir/unpack
mkdir -p "$unpack"
< "$tarball" bzcat | tar x -C "$unpack" || oops "failed to unpack '$url'"

script=$(echo "$unpack"/*/install)

[ -e "$script" ] || oops "installation script is missing from the binary tarball!"
"$script"

} # End of wrapping
