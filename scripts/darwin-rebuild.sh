#!/usr/bin/env bash

set -euo pipefail

readonly shabka_path="$(cd $(dirname "${BASH_SOURCE[0]}")/../ && pwd)"

source "${shabka_path}/lib/bash/rebuild-common.sh"

readonly darwin_config="hosts/${host}/configuration.nix"
if ! [[ -r "${darwin_config}" ]]; then
    echo "ERR: configuration for darwin_config ${darwin_config} does not exist."
    exit 1
fi

echo "NIX_PATH=$NIX_PATH"
set -x
darwin-rebuild -I "darwin-config=${darwin_config}" "${@}"
