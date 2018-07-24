#!/usr/bin/env bash
set -euo pipefail

if [[ "${#}" -eq 0 ]]; then
	echo -e "USAGE: $0 <command> [option] [option] ..."
	echo -e 'See `man 8 nixos-rebuild` for more information'
	exit 1
fi

readonly here="$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)"

cd "${here}"

readonly revision="$( git show --quiet --format='%h' )"

PS4="$ "
set -x

sudo NIXOS_LABEL_VERSION="github.com-kalbasit-system.${revision}" nixos-rebuild "${@}"
