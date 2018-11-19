#! /usr/bin/env bash

set -euo pipefail

readonly here="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
readonly root="$( cd "${here}/.." && pwd )"

if [[ "${#}" -ne 1 ]]; then
    echo "USAGE: $0 <user>"
    exit 1
fi

readonly userName="${1}"
readonly homeManagerService="result/etc/systemd/system/home-manager-${userName}.service"

if ! [[ -f "${homeManagerService}" ]]; then
    echo "ERR: ${homeManagerService} does not exist"
    exit 1
fi

readonly activationScript="$( awk '/^ExecStart=/ { split($1, a, /=/); print a[2] }' "${homeManagerService}" )"
readonly homeManagerGeneration="$( awk '/^exec / {print $2}' "${activationScript}" )"
echo "$(dirname "${homeManagerGeneration}")"
