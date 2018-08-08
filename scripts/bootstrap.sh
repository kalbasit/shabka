#!/usr/bin/env bash

set -euo pipefail

if [[ "${#}" -ne 1 ]]; then
	echo "USAGE: sudo ./scripts/bootstrap.sh <machine>"
	echo "ERR: You must provide a host to bootstrap, <machine> must exist under machines/"
	exit 1
fi

readonly here="$(cd $(dirname "${BASH_SOURCE[0]}")/.. && pwd)"
readonly machine="${1}"

if [[ $(id -u) -gt 0 ]]; then
	echo -e "must run this as root"
	exit 1
fi

NIX_PATH="nixos-config=${here}/machines/${machine}/configuration.nix"
NIX_PATH="${NIX_PATH}:nixos-hardware=${here}/external/nixos-hardware"
NIX_PATH="${NIX_PATH}:nixpkgs-overlays=${here}/overlays"
NIX_PATH="${NIX_PATH}:nixpkgs=${here}/external/nixpkgs"
NIX_PATH="${NIX_PATH}:system-path=${here}"
export NIX_PATH

nixos-rebuild switch
