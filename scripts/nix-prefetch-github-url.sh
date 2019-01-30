#! /usr/bin/env nix-shell
#! nix-shell -i bash -p jq curl

set -euo pipefail

if [[ "${#}" -ne 3 ]]; then
    >&2 echo "USAGE: $0 <owner> <repo> <branch>"
    exit 1
fi

readonly owner="${1}"
readonly repo="${2}"
readonly branch="${3}"

readonly rev="$( curl -s -X GET "https://api.github.com/repos/${owner}/${repo}/commits?sha=${branch}" -H "Accept: application/vnd.github.v3+json" | jq -r '.[0].sha' )"

readonly url="https://github.com/${owner}/${repo}/archive/${rev}.tar.gz"
readonly sha256="$( nix-prefetch-url --unpack "${url}" 2>/dev/null )"

if [[ -z "${sha256}" ]]; then
    >&2 echo "ERR: failed to capture the sha256 of the archive"
    exit 1
fi

jq '.' <(echo "{\"url\":\"${url}\",\"sha256\":\"${sha256}\",\"rev\":\"${rev}\"}")
