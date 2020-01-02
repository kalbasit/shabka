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

readonly current_uname="$(uname -s | tr -d '\n')"
readonly shabka_path="$(cd $(dirname "${BASH_SOURCE[0]}")/../ && pwd)"

build_host() {
    local host="${1}"; shift
    local release

    if ! [[ -f "${dotshabka_path}/hosts/${host}/.uname" ]]; then
        >&2 echo "WARN: The host ${host} does not define its uname via ${dotshabka_path}/hosts/${host}/.uname and cannot be built!"
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

    echo ">>> Building the host ${host}"
    echo -e "\tNIX_PATH=${nix_path}"
    NIX_PATH="${nix_path}" \
    RELEASE="release-${release/./-}" \
        nix-build --option builders '' "${dotshabka_path}/hosts/${host}" -A system "${@}"
}


if [[ "${#}" -lt 1 ]]; then
    >&2 echo "ERR: Must give a host to build"
    exit 1
fi

build_host "${@}"
