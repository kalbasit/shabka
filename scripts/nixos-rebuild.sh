#!/usr/bin/env bash

set -euo pipefail

readonly shabka_path="$(cd $(dirname "${BASH_SOURCE[0]}")/../ && pwd)"

# define all local variables
host="$( hostname -s )"
release=

while getopts ":h:r:" opt; do
    case "${opt}" in
        h)
            host="${OPTARG}"
            ;;
        r)
            release="${OPTARG}"
            ;;
        :)
            fail "Invalid option: $OPTARG requires an argument"
            ;;
        \?)
            fail "Invalid option: $OPTARG"
            ;;
    esac
done
shift $((OPTIND -1))

readonly nixos_config="hosts/${host}/configuration.nix"
if ! [[ -r "${nixos_config}" ]]; then
    echo "ERR: configuration for nixos_config ${nixos_config} does not exist."
    exit 1
fi

if [[ -z "${release}" ]]; then
    if [[ -r "hosts/${host}/release" ]]; then
        release="$( cat "hosts/${host}/release" )"
    else
        # fallback to the default release
        release="$( tr -d "\n" < "${shabka_path}/.release" )"
    fi
fi

readonly nixpkgs="$( nix-build "${shabka_path}/external" -A "nixpkgs.${release/./-}" )"

unset NIX_PATH

set -x
nixos-rebuild -I nixpkgs="${nixpkgs}" -I "nixos-config=${nixos_config}" "${@}"
