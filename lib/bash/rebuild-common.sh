#!/usr/bin/env bash

set -euo pipefail

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

if [[ -z "${release}" ]]; then
    if [[ -r "${shabka_path}/hosts/${host}/release" ]]; then
        release="$( cat "${shabka_path}/hosts/${host}/release" )"
    else
        # fallback to the default release
        release="$( tr -d "\n" < "${shabka_path}/.release" )"
    fi
fi

readonly nixpkgs="$( nix-build "${shabka_path}" -A "external.nixpkgs.release-${release/./-}.path" )"
readonly darwin="$( nix-build "${shabka_path}" -A "external.nix-darwin.path" )"

export NIX_PATH="darwin=${darwin}:nixpkgs=${nixpkgs}:shabka=${shabka_path}"
