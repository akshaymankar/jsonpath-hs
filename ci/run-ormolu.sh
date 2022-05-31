#!/usr/bin/env bash

set -eu

readonly repo=${1:?"Please provide path to repository"}

nix-env -iA nixpkgs.cachix
echo 'experimental-features = nix-command flakes' >> /etc/nix/nix.conf

cachix use jsonpath-hs

echo "Building/Downloading ormolu"
cachix watch-exec jsonpath-hs -- nix --log-format bar-with-logs build "./$repo#ormolu"

echo "Running ormolu check"
find "$repo" -name '*.hs' | xargs ./result/bin/ormolu --mode check
