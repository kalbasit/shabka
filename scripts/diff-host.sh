#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix-diff

set -euo pipefail

readonly shabka_path="$(cd $(dirname "${BASH_SOURCE[0]}")/../ && pwd)"

usage() {
    >&2 echo "USAGE: $0 <host> <base-rev> [target-rev]"
    >&2 echo "If no target-rev is given, HEAD will be used"
}

if [[ "${#}" -gt 0 ]] && [[ "${1}" = "-h" ]] && [[ "${1}" = "--help" ]]; then
    usage
    exit 0
fi

if [[ "${#}" -lt 2 ]]; then
    usage
    exit 1
fi

readonly host="${1}"
readonly base_rev="${2}"

if [[ "${#}" -ge 3 ]]; then
    readonly target_rev="${3}"
else
    readonly target_rev="$(git rev-parse HEAD)"
fi

readonly wd="$(mktemp -d)"
trap "rm -rf ${wd}" EXIT

cd "${wd}"

(
    git clone "${shabka_path}" base
    cd base
    git checkout "${base_rev}"
)

(
    git clone "${shabka_path}" target
    cd target
    git checkout "${target_rev}"
)

# build the base
if ! "${wd}/base/scripts/build-host.sh" "${host}" --out-link base-result; then
    # probably failed because the base version does not support extra
    # arguments, in this case `--out-link base-result`. Call it using the
    # script we already have in this branch now.
    "${shabka_path}/scripts/build-host.sh" --shabka-path "${wd}/base" "${host}" --out-link base-result
fi

# build the target
"${wd}/target/scripts/build-host.sh" "${host}" --out-link target-result

nix-diff "$(nix-store -q --deriver base-result)" "$(nix-store -q --deriver target-result)"
