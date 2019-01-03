#! /usr/bin/env nix-shell

#! nix-shell -I nixpkgs=./external/nixpkgs-stable.nix
#! nix-shell -i bash -p nix-prefetch-git jq

set -euo pipefail

if [[ "${#}" -ne 2 ]]; then
    echo "USAGE: $0 <url> <rev>"
    echo "Where rev is any sha1 or references (such as refs/heads/master)"
    exit 1
fi

command nix-prefetch-git "${@}" | jq --arg ref "${2}" '. + {ref: $ref}'
