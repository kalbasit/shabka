#!/usr/bin/env bash

set -euo pipefail

readonly shabka_path="$(cd $(dirname "${BASH_SOURCE[0]}")/../ && pwd)"

source "${shabka_path}/lib/bash/rebuild-common.sh"

readonly nixos_config="hosts/${host}/configuration.nix"
if ! [[ -r "${nixos_config}" ]]; then
    echo "ERR: configuration for nixos_config ${nixos_config} does not exist."
    exit 1
fi

set -x
nixos-rebuild -I "nixos-config=${nixos_config}" "${@}"
