#!/usr/bin/env sh

set -eu

readonly repo=${1:?"Please provide path to repository"}
readonly ghc=${2:?"Please provide name of the ghc"}

nix-env -iA nixpkgs.nixFlakes nixpkgs.git
echo 'experimental-features = nix-command flakes' >> /etc/nix/nix.conf

echo "Building jsonpath with ghc=$ghc"
nix --log-format bar-with-logs build "./$repo#jsonpath-$ghc"
