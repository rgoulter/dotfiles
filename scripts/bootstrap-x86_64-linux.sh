#!/usr/bin/env bash

set -ex

SYSTEM="x86_64-linux"
FLAKE_OUTPUT=".#homeConfigurations.rgoulter-${SYSTEM}.activationPackage"
nix build --no-link "${FLAKE_OUTPUT}"

"$(nix path-info ${FLAKE_OUTPUT})"/activate

# and switch with:
# home-manager switch --flake '.#rgoulter-x86_64-darwin'
