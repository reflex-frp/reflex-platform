# This file should not be run directly; it exists to share setup code
# between the various scripts in try-reflex. Before running this
# script, DIR should be defined equal to the directory containing this
# script

# The primary source repository of try-reflex
REPO="https://github.com/ryantrinkle/try-reflex"

# These are options passed to nix-instantiate and nix-shell.
readonly NIXOPTS="--option extra-binary-caches https://ryantrinkle.com:5443/ -j 8"

# The minimum required version of Nix to run this script.
readonly MIN_NIX_VERSION="1.8"

# Whether the nix script needed to be sourced - i.e. nix commands are
# not available without doing so, from the user's basic prompt.
NEEDED_TO_SOURCE_NIX_SCRIPT=0

trap "echo 'It looks like a problem occurred.  Please submit an issue at $REPO/issues'; exit 1" ERR

displayMessage() {
    cat <<EOF
If you have any trouble with this script, please submit an
issue at $REPO/issues

EOF
}

ensureNix() {
    if [[ ! -d /nix ]]; then
        if ! type -P curl >/dev/null ; then
            cat <<EOF
Please make sure that 'curl' is installed and can be run from this
shell.
EOF
            exit 1
        fi

        cat <<EOF
In order to continue, $0 must install the Nix package manager. This
requires root access (via sudo), so you will be prompted for your
password. If you do not wish to continue, just hit Ctrl-C at the
password prompt.
EOF
        "$DIR/install-nix"
    fi
}

sourceNixScript() {
    source $HOME/.nix-profile/etc/profile.d/nix.sh
}

checkNixShell() {
    if ! type -P nix-shell >/dev/null ; then
        sourceNixScript
        NEEDED_TO_SOURCE_NIX_SCRIPT=1

        if ! type -P nix-shell >/dev/null ; then
            cat <<EOF
It looks like Nix isn't working.  Please make sure you can run
nix-shell, then retry the $0, or submit an issue at
$REPO/issues"
EOF
            exit 1
        fi
    fi
}

checkNixVersion() {
    if [[ "$(nix-instantiate --eval -E "builtins.compareVersions builtins.nixVersion \"$MIN_NIX_VERSION\" >= 0")" != "true" ]]; then
        cat <<EOF
It looks like your version of Nix, $(nix-instantiate --eval -E builtins.nixVersion),
is older than the minimum version required by try-reflex,
\"$MIN_NIX_VERSION\".  You'll need to upgrade Nix to continue.  On
non-NixOS platforms, that can usually be done like this:

EOF
        if [[ "$NEEDED_TO_SOURCE_NIX_SCRIPT" -ne 0 ]] ; then
            echo "    source $HOME/.nix-profile/etc/profile.d/nix.sh"
        fi

        cat <<EOF
    nix-env --upgrade

If you're on NixOS, you may need to upgrade your OS to a later
version:

    nixos-rebuild switch --upgrade

See https://nixos.org/nixos/manual/sec-upgrading.html for more
information

If your Nix version is too old too evaluate Nixpkgs, execute:

    curl -L https://hydra.nixos.org/job/nixos/trunk-combined/nixpkgs.nix.x86_64-linux/latest-finished/nix/pkg/nix-1.10-x86_64-linux.nixpkg | nix-install-package --non-interactive -
EOF
        exit 1
    fi
}

gitThunk() {
    cat <<EOF
import ((import <nixpkgs> {}).fetchgit (import ./git.nix))
EOF
}

gitManifest() {
    local repo="$1"

    # Don't use git@github.com origins, since these can't be accessed by nix
    local url="$(git -C "$repo" config --get remote.origin.url | sed 's_^git@github.com:_git://github.com/_')"
    local rev="$(git -C "$repo" rev-parse HEAD)"
    local hash="$($(nix-build -E "(import <nixpkgs> {}).nix-prefetch-scripts")/bin/nix-prefetch-git "$PWD/$repo" "$rev" 2>/dev/null | tail -n 1)"

    cat <<EOF
{
  url = $url;
  rev = "$rev";
  sha256 = "$hash";
}
EOF
}

main() {
    displayMessage
    ensureNix
    checkNixShell
    checkNixVersion
}

main "$@"
