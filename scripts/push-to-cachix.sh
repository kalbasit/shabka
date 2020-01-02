#!/usr/bin/env bash

set -euo pipefail

# check that cachix CLI tool is installed
if ! command -v cachix >/dev/null 2>&1; then
    >&2 echo "ERR: Cachix CLI tool is required to push to Cachix!"
    exit 1
fi

# find .shabka
if [[ "x$(printenv DOTSHABKA_PATH)" == "x" ]]; then
    >&2 echo "ERR: Please define DOTSHABKA_PATH to point to the location of your .shabka"
    exit 1
fi
readonly dotshabka_path="${DOTSHABKA_PATH}"
if ! [[ -d "${dotshabka_path}" ]]; then
    >&2 echo "${dotshabka_path} No such directory"
    exit 1
fi

readonly shabka_path="$(cd $(dirname "${BASH_SOURCE[0]}")/../ && pwd)"
readonly current_uname="$(uname -s | tr -d '\n')"

# I don't want to ever make the mistake of pushing while my private files are
# accessible. This is meant to go away eventually, see #216 for more
# information
if [[ -e "${dotshabka_path}/secrets" ]]; then
    >&2 echo "ERR: ${dotshabka_path}/secrets exists. Will not continue!"
    exit 1
fi

if [[ -z "${CACHIX_SIGNING_KEY:-}" ]]; then
    >&2 echo "ERR: Please set the environment variable CACHIX_SIGNING_KEY before calling the script."
    exit 1
fi

push_host() {
    local host="${1}"
    local release

    if ! [[ -f "${dotshabka_path}/hosts/${host}/.uname" ]]; then
        >&2 echo "WARN: The host ${host} does not define its uname via hosts/${host}/.uname and cannot be built!"
        return
    fi

    local host_uname="$(tr -d '\n' < "${dotshabka_path}/hosts/${host}/.uname" )"

    if [[ -r "${dotshabka_path}/hosts/${host}/release" ]]; then
        release="$( cat "${dotshabka_path}/hosts/${host}/release" )"
    else
        # fallback to the default release
        release="$( tr -d "\n" < "${shabka_path}/.release" )"
    fi

    if [[ "${current_uname}" != "${host_uname}" ]]; then
        >&2 echo "WARN: The host ${host} of type ${host_uname} cannot be built on ${current_uname}!"
        return
    fi

    local nix_path="$( "${shabka_path}/lib/bash/nix-path.sh" "${release}" )"

    echo ">>> Building the host ${host}, and pushing to Cachix"
    echo -e "\tNIX_PATH=${nix_path}"
    NIX_PATH="${nix_path}" \
    RELEASE="release-${release/./-}" \
        nix-build --option builders '' "${dotshabka_path}/hosts/${host}" -A system | cachix push yl
}


if [[ "${#}" -eq 1 ]]; then
    push_host "${1}"
    exit
fi

for hostPath in "${dotshabka_path}"/hosts/*; do
    push_host "$(basename "${hostPath}")"
done
