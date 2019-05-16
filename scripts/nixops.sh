#!/usr/bin/env bash

set -euo pipefail

readonly shabka_path="$(cd $(dirname "${BASH_SOURCE[0]}")/../ && pwd)"

readonly release="$( tr -d "\n" < "${shabka_path}/.release" )"
source "${shabka_path}/lib/bash/rebuild-common.sh"

readonly network_secrets="${HOME}/private/network-secrets"
readonly state_location="shabka/deployments.nixops"

# make sure that there are no changes in the index
if [[ "$( git -C "${network_secrets}" diff --cached | wc -l )" -gt 0 ]]; then
    >&2 echo "ERR: you have cached changes in ${network_secrets}"
    exit 1
fi

# make sure that there are no changes to the state
if [[ "$( git -C "${network_secrets}" diff -- "${state_location}" | wc -l )" -gt 0 ]]; then
    >&2 echo "ERR: you have changes in ${network_secrets}/${state_location}"
    exit 1
fi

export NIXOPS_STATE="${network_secrets}/${state_location}";

echo "NIX_PATH=$NIX_PATH"
set +e
(
    set -x
    nixops "${@}"
    { exit_code=$?; set +x; } 2>/dev/null
    exit $exit_code
)
readonly exit_status=$?
set -e

if [[ "$( git -C "${network_secrets}" diff -- "${state_location}" | wc -l )" -eq 0 ]]; then
    >&2 echo ">>> No changes were detected to the state."
    exit 0
fi

git -C "${network_secrets}" add -A "${state_location}"
git -C "${network_secrets}" commit \
    -m "shabka: automatic update of the deployments via nixops wrapper" \
    -m "Command: nixops ${*}" \
    -m "Exit code: ${exit_status}"

>&2 echo ">>> Do not forget to push your Git repository with the state!"
