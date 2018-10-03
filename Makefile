.PHONY: all update update-nixpkgs

HOSTNAME ?= $(shell hostname -s)

NIXOS_REBUILD=./lib/shell/bin/nixos-rebuild.sh
NIXOS_REBUILD_OPERATIONS = switch boot test build dry-build dry-activate build-vm build-vm-with-bootloader
.PHONY: $(NIXOS_REBUILD_OPERATIONS)

all: build

$(NIXOS_REBUILD_OPERATIONS): add-channels update-channels
	@echo ">>> Running nixos-rebuild $@"
	@$(NIXOS_REBUILD) $(HOSTNAME) $@

add-channels:
	@echo ">>> Adding all the relevant channels, this will override any previously added channel if the version is different"
	@nix-channel --add https://nixos.org/channels/nixos-unstable nixpkgs-unstable
	@nix-channel --add https://nixos.org/channels/nixos-18.09 nixpkgs
	@nix-channel --add https://github.com/rycee/home-manager/archive/release-18.09.tar.gz home-manager
	@nix-channel --add https://github.com/NixOS/nixos-hardware/archive/master.tar.gz nixos-hardware

update-channels:
	@echo ">>> Updating all channels"
	@nix-channel --update
