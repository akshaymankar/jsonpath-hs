#!/usr/bin/env bash

set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

nix build -f "$DIR/flake.nix" -o "$DIR/.direnv"
env="$PWD/.direnv"

eval "$(direnv stdlib)"
load_prefix "$env"

haskell-language-server-wrapper "$@"
