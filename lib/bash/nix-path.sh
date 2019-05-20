#!/usr/bin/env bash

set -euo pipefail

readonly shabka_path="$(cd $(dirname "${BASH_SOURCE[0]}")/../../ && pwd)"

if [[ "${#}" -ne 1 ]]; then
    >&2 echo "USAGE: $0 <release>"
fi

readonly release="${1}"

readonly nixpkgs="$( nix-build --no-out-link "${shabka_path}" -A "external.nixpkgs.release-${release/./-}.path" )"
readonly darwin="$( nix-build --no-out-link "${shabka_path}" -A "external.nix-darwin.path" )"

echo "darwin=${darwin}:nixpkgs=${nixpkgs}:shabka=${shabka_path}"
