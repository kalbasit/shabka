#!/usr/bin/env bash

# TODO: add support for doing the same thing for Mac as well!

set -euo pipefail

readonly shabka_path="$(cd $(dirname "${BASH_SOURCE[0]}")/../ && pwd)"
readonly nixpkgs_stable="${shabka_path}/external/nixpkgs-stable.nix"
readonly nixpkgs_unstable="${shabka_path}/external/nixpkgs-unstable.nix"

# define all local variables
host="$( hostname -s )"
release="stable"

while getopts ":h:u" opt; do
    case "${opt}" in
        h)
            host="${OPTARG}"
            ;;
        u)
            release="unstable"
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

if [[ "${release}" = "stable" ]]; then
    readonly nixpkgs="$( nix-build --no-out-link ${nixpkgs_stable} )"
else
    # TODO: improve the beild command
    readonly nixpkgs="$( nix-build --no-out-link ./external/nixpkgs-unstable.nix --arg runCommand '(import <nixpkgs> {}).runCommand' --arg fetchpatch '(import <nixpkgs> {}).fetchpatch' )"
fi

# use nix-build to get the nixpkgs source path in the nix store. nix-shell can
# be pointed to nixpkgs.nix as is and it's able to call the function to get the
# actual source but for some reason this is not work with nixos-rebuild. See
# https://gist.github.com/kalbasit/deec7b74b64f70d24ca1967883c8e7b6 for more
# details.

unset NIX_PATH

set -x
nixos-rebuild -I nixpkgs="${nixpkgs}" -I "nixos-config=${nixos_config}" "${@}"
