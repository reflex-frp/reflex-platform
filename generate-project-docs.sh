#!/usr/bin/env bash
set -euo pipefail

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

. "$DIR/common-setup.sh"

cat $(nix-build --no-out-link -E "((import $DIR {}).project ({ ... }: {})).config.options-docs-md") > docs/project-options.md
