#!/usr/bin/env bash

set -euo pipefail

readonly current_uname="$(uname -s | tr -d '\n')"

build_host() {
    local host="${1}"; shift
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

    local nix_path="$( "${shabka_path}/lib/bash/nix-path.sh" "${release}" )"

    echo ">>> Building the host ${host}"
    echo -e "\tNIX_PATH=${nix_path}"
    NIX_PATH="${nix_path}" \
        nix-build --option builders '' "${shabka_path}/hosts/${host}" -A system "${@}"
}


if [[ "${#}" -lt 1 ]]; then
    >&2 echo "ERR: Must give a host to build"
    exit 1
fi

if [[ "${1}" == "--shabka-path" ]]; then
    readonly shabka_path="${2}"
    shift 2
else
    readonly shabka_path="$(cd $(dirname "${BASH_SOURCE[0]}")/../ && pwd)"
fi


build_host "${@}"
