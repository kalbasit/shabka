#!/usr/bin/env bash

# TODO: add support for doing the same thing for Mac as well!

set -euo pipefail

readonly shabka_path="$(cd $(dirname "${BASH_SOURCE[0]}")/../ && pwd)"

# use nix-build to get the nixpkgs source path in the nix store. nix-shell can
# be pointed to nixpkgs.nix as is and it's able to call the function to get the
# actual source but for some reason this is not work with nixos-rebuild. See
# https://gist.github.com/kalbasit/deec7b74b64f70d24ca1967883c8e7b6 for more
# details.
readonly nixpkgs_stable="${shabka_path}/external/nixpkgs-stable.nix"
readonly nixpkgs="$( nix-build --no-out-link ${nixpkgs_stable} )"

export NIX_PATH="nixpkgs=${nixpkgs}"
export NIXOPS_STATE="${HOME}/keybase/private/ylcodes/system/deployments.nixops";

set -x
nixops "${@}"
