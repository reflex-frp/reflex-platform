#!/bin/sh

# This script installs the Nix package manager on your system by
# downloading a binary distribution and running its installer script
# (which in turn creates and populates /nix).

{ # Prevent execution if this script was only partially downloaded

# TODO (ebzzry): use a better way to determine unpack directory
UNPACK=nix-binary-tarball-unpack

NIX_URL="https://nixos.org/releases/nix/nix-1.10/nix-1.10-$SYSTEM.tar.bz2"
SYSTEM=

oops() {
    echo "$0: $@" >&2
    rm -rf "$UNPACK"
    exit 1
}

getSystem() {
    case "$(uname -s).$(uname -m)" in
        Linux.x86_64) SYSTEM=x86_64-linux;;
        Linux.i?86) SYSTEM=i686-linux;;
        Darwin.x86_64) SYSTEM=x86_64-darwin;;
        # TODO (ebzzry): check status of Nix on FreeBSD
        *) oops "Sorry, there is no binary distribution of Nix for your platform";;
    esac
}

requireUtil() {
    type "$1" > /dev/null 2>&1 || which "$1" > /dev/null 2>&1 ||
        oops "You do not have \`$1' installed, which i need to $2"
}

ensureUtils() {
    requireUtil curl "download the binary tarball"
    requireUtil bzcat "decompress the binary tarball"
    requireUtil tar "unpack the binary tarball"
}

installNix() {
    [ -e "$UNPACK"/*/install ] ||
        oops "Installation script is missing from the binary tarball!"

    "$UNPACK"/*/install
}

unpackTarball() {
    echo "Unpacking Nix binary tarball for $SYSTEM from $NIX_URL..."
    mkdir "$UNPACK" || oops "Failed to create \`$UNPACK' directory"
    curl -L "$NIX_URL" | bzcat | tar x -C "$UNPACK" \
        || oops "Failed to unpack \`$NIX_URL'"

}

cleanUp() {
    rm -rf "$UNPACK"
}

main() {
    getSystem
    ensureUtils
    unpackTarball
    installNix
    cleanUp
}

main "$@"

} # End of wrapping
