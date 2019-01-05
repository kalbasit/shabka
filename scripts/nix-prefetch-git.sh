#! /usr/bin/env nix-shell

#! nix-shell -i bash -p nix-prefetch-git jq

# TODO: The purpose of this script is to print the ref (if not a commit) as
# part of the JSON. This should be done upstream so subit a PR for it!

set -euo pipefail

if [[ "${#}" -ne 2 ]]; then
    echo "USAGE: $0 <url> <rev>"
    echo "Where rev is any sha1 or references (such as refs/heads/master)"
    exit 1
fi

command nix-prefetch-git "${@}" | jq --arg ref "${2}" '. + {ref: $ref}'
