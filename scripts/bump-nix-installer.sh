#!/usr/bin/env bash
set -euo pipefail

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." && pwd )

echo "Updating installNix.sh with the latest official version"
echo ""
curl https://nixos.org/nix/install > $DIR/scripts/installNix.sh
echo ""
echo "Done"
echo "Consider committing installNix.sh"
