#!/usr/bin/env bash

# NOTE: this script should not use nix-shell

set -euo pipefail

readonly SYSTEM_PATH="$(cd $(dirname "${BASH_SOURCE[0]}")/../../.. && pwd)"

source "${SYSTEM_PATH}/lib/shell/functions/compute_nix_path.sh"

if [[ "${#}" -lt 2 ]]; then
	echo "USAGE: ${BASH_SOURCE[0]} <machine> <action>"
	echo "ERR: You must provide a host to bootstrap, <machine> must exist under nixos/machines/"
	exit 1
fi

readonly hostname="$(hostname -s)"
readonly machine="${1}"
readonly action="${2}"

if [[ ! -r "${SYSTEM_PATH}/nixos/machines/${machine}/configuration.nix" ]]; then
	echo "ERR: configuration for machine ${machine} does not exist."
	exit 1
fi

if [[ "${machine}" != "${hostname}" ]]; then
	if [[ "${action}" == "test" ]] || [[ "${action}" == "switch" ]]; then
		echo "ERR: cannot run \'nixos-rebuild ${action}\` for ${machine} from ${hostname}."
		exit 1
	fi
fi

NIX_PATH="$(compute_nix_path)"
export NIX_PATH

if [[ $(id -u) -gt 0 ]] && ([[ "${action}" == "switch" ]] || [[ "${action}" == "test" ]] || [[ "${action}" == "boot" ]]) ; then
	echo -e "ERR: must run this as root"
	exit 1
fi

nixos-rebuild "${action}"
