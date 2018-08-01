#!/usr/bin/env bash

if [[ $(id -u) -gt 0 ]]; then
	echo -e "must run this as root"
	exit 1
fi

export nixpkgs=/code/personal/base/src/github.com/kalbasit/system/external/nixpkgs:nixos-config=/code/personal/base/src/github.com/kalbasit/system/machines/hades/configuration.nix:nixos-hardware=/code/personal/base/src/github.com/kalbasit/system/external/nixos-hardware

nixos-rebuild switch

su kalbasit -c '
set -euo pipefail

nix-shell -p home-manager --run 'home-manager build'
'
