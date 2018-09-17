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

readonly machine="${1}"
readonly action="${2}"

if [[ ! -r "${SYSTEM_PATH}/nixos/machines/${machine}/configuration.nix" ]]; then
	echo "ERR: configuration for machine ${machine} does not exist."
	exit 1
fi

NIX_PATH="$(compute_nix_path)"
export NIX_PATH

if [[ ! -r "${HOME}/.config/nixpkgs/home.nix" ]]; then
	mkdir -p "${HOME}/.config/nixpkgs"
	cat <<EOF > "${HOME}/.config/nixpkgs/home.nix"
{
  imports = [
    <system-path/home/cfg>
  ];
}
EOF
fi

if ! command -v home-manager &>/dev/null; then
	nix-env -iA home-manager
fi

home-manager "${action}"
