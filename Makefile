.PHONY: all update update-nixpkgs

HOSTNAME ?= $(shell hostname -s)

NIXOS_REBUILD=./lib/shell/bin/nixos-rebuild.sh
NIXOS_REBUILD_OPERATIONS = switch boot test build dry-build dry-activate build-vm build-vm-with-bootloader
.PHONY: $(NIXOS_REBUILD_OPERATIONS)

all: build

$(NIXOS_REBUILD_OPERATIONS):
	@$(NIXOS_REBUILD) $(HOSTNAME) $@

update: update-nixpkgs update-nixos-hardware update-home-manager

update-nixpkgs:
	nix-shell -p nix-prefetch-git --run 'nix-prefetch-git https://github.com/NixOS/nixpkgs-channels.git refs/heads/nixos-unstable' > external/nixpkgs-version.json

update-nixos-hardware:
	nix-shell -p nix-prefetch-git --run 'nix-prefetch-git https://github.com/NixOS/nixos-hardware.git refs/heads/master' > external/nixos-hardware-version.json

update-home-manager:
	nix-shell -p nix-prefetch-git --run 'nix-prefetch-git https://github.com/rycee/home-manager.git refs/heads/master' > external/home-manager-version.json
