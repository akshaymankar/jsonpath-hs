#!/usr/bin/env sh

set -eu

readonly repo=${1:?"Please provide path to repository"}
readonly ghc=${2:?"Please provide name of the ghc"}

nix-env -iA nixpkgs.nixFlakes nixpkgs.git nixpkgs.cachix
echo 'experimental-features = nix-command flakes' >> /etc/nix/nix.conf


# TODO: Delete this when next version of nixos/nix image is published.
mkdir -p /etc/ssl/certs
ln -s "$NIX_SSL_CERT_FILE" /etc/ssl/certs

cachix use jsonpath-hs

echo "Building jsonpath with ghc=$ghc"
cachix watch-exec jsonpath-hs -- nix --log-format bar-with-logs build "./$repo#jsonpath-$ghc"
