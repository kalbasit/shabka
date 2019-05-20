#! /usr/bin/env nix-shell
#! nix-shell -i bash -p jq git

set -euo pipefail

if [[ "${#}" -ne 4 ]]; then
    >&2 echo "USAGE: $0 <owner> <repo> <branch> <external_file>"
    exit 1
fi

# make sure that there are no changes in the index
if [[ "$( git diff --cached | wc -l )" -gt 0 ]]; then
    >&2 echo "ERR: you have cached changes"
    exit 1
fi

readonly root_dir="$( cd "$(dirname ${BASH_SOURCE[0]})/.." && pwd )"

readonly owner="${1}"
readonly repo="${2}"
readonly branch="${3}"
readonly external_file="${4}"
readonly tmp="$(mktemp)"

if [[ -r "${external_file}" ]]; then
    readonly new=0
    readonly old_rev="$(jq -r '.rev' "${external_file}")"
else
    readonly new=1
fi

# make sure that there are no changes to the state
if [[ "$( git diff -- "${external_file}" | wc -l )" -gt 0 ]]; then
    >&2 echo "ERR: you have changes in ${external_file}"
    exit 1
fi

${root_dir}/scripts/nix-prefetch-github-url.sh "${owner}" "${repo}" "${branch}" > "${tmp}" && mv "${tmp}" "${external_file}"

if [[ "${new}" -eq 0 ]] && [[ "$( git  diff -- "${external_file}" | wc -l )" -eq 0 ]]; then
    >&2 echo ">>> No changes were detected at ${external_file}."
    exit 0
fi

readonly new_rev="$(jq -r '.rev' "${external_file}")"

git add -A "${external_file}"

external_module="${external_file%%/version.json}"
external_module="${external_module##${root_dir}/}"

if [[ "${new}" -eq 0 ]]; then
    git commit -m "${external_module}: update to ${new_rev}" \
        -m "Compare changes at https://github.com/${owner}/${repo}/compare/${old_rev}...${new_rev}"
else
    git commit -m "${external_module}: update to ${new_rev}"
fi
