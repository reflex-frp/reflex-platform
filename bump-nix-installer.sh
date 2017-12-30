#!/usr/bin/env bash
set -euo pipefail

echo "Updating installNix.sh with the latest official version"
echo ""
curl https://nixos.org/nix/install > installNix.sh
echo ""
echo "Done"
echo "Consider committing installNix.sh"
