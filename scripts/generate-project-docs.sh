#!/usr/bin/env bash
set -euo pipefail

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

. "$DIR/common-setup.sh"

cat $(nix-build --no-out-link -E "((import $DIR/../default.nix {}).project ({ ... }: {})).config.options-docs-md") > docs/project-options.md

cat $(nix-build --no-out-link -E "((import $DIR/../default.nix {}).project ({ ... }: {})).config.project-dev-docs-md $DIR/../docs/project-development-contents.md") > docs/project-development.md
