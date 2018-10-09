#!/usr/bin/env bash

# NOTE: this script should not use nix-shell

set -euo pipefail

readonly SYSTEM_PATH="$(cd $(dirname "${BASH_SOURCE[0]}")/../../.. && pwd)"

source "${SYSTEM_PATH}/lib/shell/functions/compute_nix_path.sh"
source "${SYSTEM_PATH}/lib/shell/functions/require_clean_work_tree.sh"

if [[ "${#}" -lt 2 ]]; then
	echo "USAGE: ${BASH_SOURCE[0]} <machine> <command>"
	echo "ERR: You must provide a host to bootstrap, <machine> must exist under nixos/machines/"
	exit 1
fi

readonly hostname="$(hostname -s)"
readonly machine="${1}"
readonly command="${2}"

if [[ ! -r "${SYSTEM_PATH}/hosts/${machine}/default.nix" ]]; then
	echo "ERR: configuration for machine ${machine} does not exist."
	exit 1
fi

if [[ "${machine}" != "${hostname}" ]]; then
	if [[ "${command}" == "test" ]] || [[ "${command}" == "switch" ]]; then
		echo "ERR: cannot run \'nixos-rebuild ${command}\` for ${machine} from ${hostname}."
		exit 1
	fi
fi

NIX_PATH="$(compute_nix_path)"
export NIX_PATH

if [[ $(id -u) -gt 0 ]] && ([[ "${command}" == "switch" ]] || [[ "${command}" == "test" ]] || [[ "${command}" == "boot" ]]) ; then
	echo -e "ERR: must run this as root"
	exit 1
fi

if [[ "${command}" == "switch" ]] || [[ "${command}" == "boot" ]]; then
	require_clean_work_tree ${command} "Please commit or stash all changes and try again."
fi

nixos-rebuild "${command}"
