#!/usr/bin/env bash
set -euo pipefail

if ! nix-channel --list | grep -q -- -unstable; then
	echo "Subscribing to the unstable channel"

	if grep -q NixOS /etc/os-release; then
		nix-channel --add https://nixos.org/channels/nixos-unstable
	else
		nix-channel --add https://nixos.org/channels/nixpkgs-unstable
	fi

	nix-channel --update
fi
