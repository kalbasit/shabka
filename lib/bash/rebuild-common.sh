#!/usr/bin/env bash

set -euo pipefail

# define all local variables
host="$( hostname -s )"
if ! [[ -v release ]] || [[ -z "${release:-}" ]]; then
    release=
fi

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
            # end of possible arguments, rest are for the command
            break
            ;;
    esac
done
shift $((OPTIND -1))

if [[ -z "${release}" ]]; then
    if [[ -r "${dotshabka_path}/hosts/${host}/release" ]]; then
        release="$( cat "${dotshabka_path}/hosts/${host}/release" )"
    else
        # fallback to the default release
        release="$( tr -d "\n" < "${shabka_path}/.release" )"
    fi
fi

export RELEASE="release-${release/./-}"
export NIX_PATH="$( "${shabka_path}/lib/bash/nix-path.sh" "${release}" )"
