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

readonly darwin_config="${dotshabka_path}/hosts/${host}/configuration.nix"
if ! [[ -r "${darwin_config}" ]]; then
    echo "ERR: configuration for darwin_config ${darwin_config} does not exist."
    exit 1
fi

echo "NIX_PATH=$NIX_PATH"
set -x
darwin-rebuild -I "darwin-config=${darwin_config}" "${@}"
