# This file should not be run directly; it exists to share setup code between the various scripts in try-reflex
# Before running this script, DIR should be defined equal to the directory containing this script

REPO="https://github.com/ryantrinkle/try-reflex"

NIXOPTS="--option extra-binary-caches https://ryantrinkle.com:5443/ -j 8"

trap "echo 'It looks like a problem occurred.  Please submit an issue at $REPO/issues'; exit 1" ERR

echo "If you have any trouble with this script, please submit an issue at $REPO/issues"

(

cd "$DIR"

if [ ! -d /nix ] ; then
  if ! type -P curl >/dev/null ; then
    echo "Please make sure that 'curl' is installed and can be run from this shell"
    exit 1
  fi

  echo "In order to continue, $0 must install the Nix package manager.  This requires root access, so you will be prompted for your password.  If you do not wish to continue, just hit Ctrl-C at the password prompt."
  ./installNix.sh
fi

)

# The command to source the nix script.  This should be a line of valid bash code.
SOURCE_NIX_SCRIPT=". $HOME/.nix-profile/etc/profile.d/nix.sh"

# Whether the nix script needed to be sourced - i.e. nix commands are not available without doing so, from the user's basic prompt.
NEEDED_TO_SOURCE_NIX_SCRIPT=0

if ! type -P nix-shell >/dev/null ; then
  $SOURCE_NIX_SCRIPT
  NEEDED_TO_SOURCE_NIX_SCRIPT=1
  if ! type -P nix-shell >/dev/null ; then
    echo "It looks like Nix isn't working.  Please make sure you can run nix-shell, then retry the $0, or submit an issue at $REPO/issues"
    exit 1
  fi
fi

# The minimum required version of Nix to run this script.
MIN_REQUIRED_NIX_VERSION="1.8"

if [ "$(nix-instantiate --eval --expr "builtins.compareVersions builtins.nixVersion \"$MIN_REQUIRED_NIX_VERSION\" >= 0")" != "true" ] ; then
  echo "It looks like your version of Nix, $(nix-instantiate --eval --expr "builtins.nixVersion"), is older than the minimum version required by try-reflex, \"$MIN_REQUIRED_NIX_VERSION\".  You'll need to upgrade Nix to continue.  On non-NixOS platforms, that can usually be done like this:"
  if [ "$NEEDED_TO_SOURCE_NIX_SCRIPT" -ne 0 ] ; then
    echo "$SOURCE_NIX_SCRIPT"
  fi
  echo "nix-env --upgrade"
  echo "If you're on NixOS, you may need to upgrade your OS to a later version.  See https://nixos.org/nixos/manual/sec-upgrading.html"
  exit 1
fi

(

cd "$DIR"


if ! type -P git >/dev/null ; then
  echo "Please make sure that 'git' is installed and can be run from this shell"
  exit 1
fi

for x in nixpkgs reflex reflex-dom reflex-todomvc ; do
  if [ ! "$(ls -A "$x")" ] ; then

    git submodule update --init --recursive "$x"
  fi
done

)
