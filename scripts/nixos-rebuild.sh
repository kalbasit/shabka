#!/usr/bin/env bash

# TODO: add support for doing the same thing for Mac as well!

set -euo pipefail

if [[ "${#}" -lt 1 ]]; then
    echo "USAGE: ${BASH_SOURCE[0]} <action>"
    echo "ERR: You must provide an <action>."
    exit 1
fi

readonly shabka_path="$(cd $(dirname "${BASH_SOURCE[0]}")/../ && pwd)"

readonly action="${1}"
shift

readonly host="$( hostname -s )"
readonly nixos_config="${shabka_path}/hosts/${host}/configuration.nix"

if ! [[ -r "${nixos_config}" ]]; then
    echo "ERR: configuration for nixos_config ${nixos_config} does not exist."
    exit 1
fi

# use nix-build to get the nixpkgs source path in the nix store. nix-shell can
# be pointed to nixpkgs.nix as is and it's able to call the function to get the
# actual source but for some reason this is not work with nixos-rebuild. See
# https://gist.github.com/kalbasit/deec7b74b64f70d24ca1967883c8e7b6 for more
# details.
readonly nixpkgs_stable="${shabka_path}/external/nixpkgs-stable.nix"
readonly nixpkgs="$( nix-build --no-out-link ${nixpkgs_stable} )"

unset NIX_PATH

set -x
nixos-rebuild -I nixpkgs="${nixpkgs}" -I "nixos-config=${nixos_config}" "${action}" "${@}"
