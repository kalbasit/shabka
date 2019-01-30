#! /usr/bin/env nix-shell
#! nix-shell -i bash -p jq curl nix-prefetch-scripts

set -euo pipefail

if [[ "${#}" -ne 3 ]]; then
    echo "USAGE: $0 <owner> <repo> <branch>"
    exit 1
fi

readonly owner="${1}"
readonly repo="${2}"
readonly branch="${3}"

readonly latest_commit="$( curl -s -X GET "https://api.github.com/repos/${owner}/${repo}/commits?sha=${branch}" -H "Accept: application/vnd.github.v3+json" | jq -r '.[0].sha' )"

readonly url="https://github.com/${owner}/${repo}/archive/${latest_commit}.tar.gz"
readonly sha256="$( nix-prefetch-url "${url}" )"

echo "{\"url\":\"${url}\",\"sha256\":\"${sha256}\"}" | jq
