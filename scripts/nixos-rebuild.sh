#!/usr/bin/env bash

# TODO: add support for doing the same thing for Mac as well!

set -euo pipefail

if [[ "${#}" -lt 2 ]]; then
    echo "USAGE: ${BASH_SOURCE[0]} <nixos_config> <action>"
    echo "ERR: You must provide <nixos_config> and an <action>."
    exit 1
fi

readonly nixos_config="${1}"
readonly action="${2}"
shift 2

readonly shabka_path="$(cd $(dirname "${BASH_SOURCE[0]}")/../ && pwd)"

if ! [[ -r "${nixos_config}" ]]; then
    echo "ERR: configuration for nixos_config ${nixos_config} does not exist."
    exit 1
fi

# use nix eval to get the nixpkgs source path in the nix store. nix-shell can
# be pointed to nixpkgs.nix as is and it's able to call the function to get the
# actual source but for some reason this is not work with nixos-rebuild. See
# https://gist.github.com/kalbasit/deec7b74b64f70d24ca1967883c8e7b6 for more
# details.
readonly nixpkgs_stable="${shabka_path}/external/nixpkgs-stable.nix"
readonly nixpkgs="$( nix-build --no-out-link ${nixpkgs_stable} )"

unset NIX_PATH

set -x
nixos-rebuild -I nixpkgs="${nixpkgs}" -I "nixos-config=${nixos_config}" "${action}" "${@}"
