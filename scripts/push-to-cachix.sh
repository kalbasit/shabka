#!/usr/bin/env bash

set -euo pipefail

readonly shabka_path="$(cd $(dirname "${BASH_SOURCE[0]}")/../ && pwd)"

# I don't want to ever make the mistake of pushing while my private files are
# accessible. This is meant to go away eventually, see #216 for more
# information
if [[ -e "${shabka_path}/secrets.nix" ]]; then
    >&2 echo "ERR: ${shabka_path}/secrets.nix exists. Will not continue!"
    exit 1
fi

if [[ -z "${CACHIX_SIGNING_KEY:-}" ]]; then
	>&2 echo "ERR: Please set the environment variable CACHIX_SIGNING_KEY before calling the script."
    exit 1
fi

if [[ "${#}" -eq 1 ]]; then
    readonly host="${1}"
    echo "Pushing the cache for ${host}"
    nix-build --option builders '' "${shabka_path}/hosts/${host}" -A nixos | cachix push yl
else
    for hostPath in "${shabka_path}"/hosts/*; do
        readonly host="$(basename "${hostPath}")"
        if grep -q '\<nixos\>' "${host}/default.nix"; then
            echo "Pushing the cache for ${host}"
            nix-build --option builders '' "${shabka_path}/hosts/${host}" -A nixos | cachix push yl
        fi
    done
fi
