#!/usr/bin/env bash

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
