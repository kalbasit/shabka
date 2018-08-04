#!/usr/bin/env bash

readonly here="$(cd $(dirname "${BASH_SOURCE[0]}")/.. && pwd)"

if [[ $(id -u) -gt 0 ]]; then
	echo -e "must run this as root"
	exit 1
fi

export nixpkgs=/code/personal/base/src/github.com/kalbasit/system/external/nixpkgs:nixos-config=/code/personal/base/src/github.com/kalbasit/system/machines/hades/configuration.nix:nixos-hardware=/code/personal/base/src/github.com/kalbasit/system/external/nixos-hardware

nixos-rebuild switch

su kalbasit -c '
set -euo pipefail

if [[ -e "${HOME}/.config/nixpkgs" ]] && [[ ! -L "${HOME}/.config/nixpkgs" ]]; then
	echo -e "${HOME}/.config/nixpkgs already exists, cannot continue."
	exit 1
fi

if [[ -L "${HOME}/.config/nixpkgs" ]] && [[ "$(readlink -f "${HOME}/.config/nixpkgs")" != "${here}" ]]; then
	echo -e "${HOME}/.config/nixpkgs already exists as a link, but does not point here, cannot continue."
	exit 1
fi

if [[ ! -e "${HOME}/.config/nixpkgs" ]]; then
	ln -s "${here}" "${HOME}/.config/nixpkgs"
fi

nix-shell -p home-manager --run 'home-manager switch'
'
