#!/usr/bin/env bash

set -ex

FLAKE_URI="."
SYSTEM="x86_64-darwin"
HM_USERNAME="richardgoulter"
HM_CONFIG="${HM_USERNAME}-${SYSTEM}"
FLAKE_OUTPUT="${FLAKE_URI}#homeConfigurations.${HM_CONFIG}.activationPackage"

nix build --no-link "${FLAKE_OUTPUT}"

"$(nix path-info ${FLAKE_OUTPUT})"/activate

# and switch with:
# home-manager switch --flake '.#richardgoulter-x86_64-darwin'
