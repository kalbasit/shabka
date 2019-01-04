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

readonly nixpkgs_stable="${shabka_path}/external/nixpkgs-stable.nix"
readonly nixpkgs="$( nix eval --raw "(import ${nixpkgs_stable} { importPinned = false; })" )"

nixos-rebuild -I nixpkgs="${nixpkgs}" -I "nixos_config=${nixos_config}" "${action}" "${@}"
