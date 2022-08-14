#!/usr/bin/env bash

set -ex

SYSTEM="x86_64-darwin"
FLAKE_OUTPUT=".#homeConfigurations.richardgoulter-${SYSTEM}.activationPackage"
nix build --no-link "${FLAKE_OUTPUT}"

"$(nix path-info ${FLAKE_OUTPUT})"/activate

# and switch with:
# home-manager switch --flake '.#rgoulter-x86_64-darwin'
