#!/usr/bin/env bash

set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

direnv exec "$DIR" haskell-language-server-wrapper "$@"
