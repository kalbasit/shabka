#!/usr/bin/env bash

set -euo pipefail

if [[ "${#}" -lt 1 ]]; then
	echo "USAGE: sudo ./scripts/nixos-install.sh <machine>"
	echo "ERR: You must provide a host to bootstrap, <machine> must exist under nixos/machines/"
	exit 1
fi

readonly here="$(cd $(dirname "${BASH_SOURCE[0]}")/.. && pwd)"
readonly machine="${1}"; shift

if [[ ! -r "${here}/nixos/machines/${machine}/configuration.nix" ]]; then
	echo "ERR: configuration for machine ${machine} does not exist."
	exit 1
fi

if [[ $(id -u) -gt 0 ]]; then
	echo -e "must run this as root"
	exit 1
fi

NIX_PATH=""
NIX_PATH="${NIX_PATH}:home-manager=${here}/external/home-manager"
NIX_PATH="${NIX_PATH}:nixos-config=${here}/nixos/machines/${machine}/configuration.nix"
NIX_PATH="${NIX_PATH}:nixos-hardware=${here}/external/nixos-hardware"
NIX_PATH="${NIX_PATH}:nixpkgs-overlays=${here}/overlays"
NIX_PATH="${NIX_PATH}:nixpkgs=${here}/external/nixpkgs"
NIX_PATH="${NIX_PATH}:system-path=${here}"
export NIX_PATH

NIXOS_CONFIG="${here}/nixos/machines/${machine}/configuration.nix"
export NIXOS_CONFIG

echo "NIX_PATH=$NIX_PATH"
echo "NIXOS_CONFIG=$NIXOS_CONFIG"
nixos-install $@
