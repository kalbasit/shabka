#!/usr/bin/env bash

set -euo pipefail

readonly shabka_path="$(cd $(dirname "${BASH_SOURCE[0]}")/../ && pwd)"
readonly current_uname="$(uname -s | tr -d '\n')"

# I don't want to ever make the mistake of pushing while my private files are
# accessible. This is meant to go away eventually, see #216 for more
# information
if [[ -e "${shabka_path}/secrets" ]]; then
    >&2 echo "ERR: ${shabka_path}/secrets exists. Will not continue!"
    exit 1
fi

if [[ -z "${CACHIX_SIGNING_KEY:-}" ]]; then
    >&2 echo "ERR: Please set the environment variable CACHIX_SIGNING_KEY before calling the script."
    exit 1
fi

push_host() {
    local host="${1}"
    local release

    if ! [[ -f "${shabka_path}/hosts/${host}/.uname" ]]; then
        >&2 echo "WARN: The host ${host} does not define its uname via hosts/${host}/.uname and cannot be built!"
        return
    fi

    local host_uname="$(tr -d '\n' < "${shabka_path}/hosts/${host}/.uname" )"

    if [[ -r "${shabka_path}/hosts/${host}/release" ]]; then
        release="$( cat "${shabka_path}/hosts/${host}/release" )"
    else
        # fallback to the default release
        release="$( tr -d "\n" < "${shabka_path}/.release" )"
    fi

    if [[ "${current_uname}" != "${host_uname}" ]]; then
        >&2 echo "WARN: The host ${host} of type ${host_uname} cannot be built on ${current_uname}!"
        return
    fi

    echo "Pushing the cache for ${host}"
    NIX_PATH="$( "${shabka_path}/lib/bash/nix-path.sh" "${release}" )" \
        nix-build --option builders '' "${shabka_path}/hosts/${host}" -A system | cachix push yl
}


if [[ "${#}" -eq 1 ]]; then
    push_host "${1}"
    exit
fi

for hostPath in "${shabka_path}"/hosts/*; do
    push_host "$(basename "${hostPath}")"
done
