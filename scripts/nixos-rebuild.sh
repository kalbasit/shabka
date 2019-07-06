#!/usr/bin/env bash

set -euo pipefail

# find .shabka
if [[ "x$(printenv DOTSHABKA_PATH)" == "x" ]]; then
    >&2 echo "Please define DOTSHABKA_PATH to point to the location of your .shabka"
    exit 1
fi
readonly dotshabka_path="${DOTSHABKA_PATH}"
if ! [[ -d "${dotshabka_path}" ]]; then
    >&2 echo "${dotshabka_path} No such directory"
    exit 1
fi

readonly shabka_path="$(cd $(dirname "${BASH_SOURCE[0]}")/../ && pwd)"

source "${shabka_path}/lib/bash/rebuild-common.sh"

readonly nixos_config="${dotshabka_path}/hosts/${host}/configuration.nix"
if ! [[ -r "${nixos_config}" ]]; then
    echo "ERR: configuration for nixos_config ${nixos_config} does not exist."
    exit 1
fi

echo "NIX_PATH=$NIX_PATH"
set -x
nixos-rebuild -I "nixos-config=${nixos_config}" "${@}"
