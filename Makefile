.PHONY: all build test switch update update-nixpkgs

HOSTNAME ?= $(shell hostname -s)

NIXOS_REBUILD=./lib/shell/bin/nixos-rebuild.sh

all: build

build:
	@$(NIXOS_REBUILD) $(HOSTNAME) build

test:
	@$(NIXOS_REBUILD) $(HOSTNAME) test

switch:
	@$(NIXOS_REBUILD) $(HOSTNAME) switch

update: update-nixpkgs update-nixos-hardware update-home-manager

update-nixpkgs:
	nix-shell -p nix-prefetch-git --run 'nix-prefetch-git https://github.com/NixOS/nixpkgs-channels.git refs/heads/nixos-unstable' > external/nixpkgs-version.json

update-nixos-hardware:
	nix-shell -p nix-prefetch-git --run 'nix-prefetch-git https://github.com/NixOS/nixos-hardware.git refs/heads/master' > external/nixos-hardware-version.json

update-home-manager:
	nix-shell -p nix-prefetch-git --run 'nix-prefetch-git https://github.com/rycee/home-manager.git refs/heads/master' > external/home-manager-version.json
