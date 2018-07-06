#!/usr/bin/env bash
set -euo pipefail

echo "Subscribing to the unstable channel"
if ! nix-channel --list | grep -q /nixpkgs-unstable; then
	nix-channel --add https://nixos.org/channels/nixpkgs-unstable
fi
