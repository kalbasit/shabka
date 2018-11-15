.PHONY: all build test switch boot brew add-channels update-channels \
	update-external update-home-manager update-nixpkgs update-nixos-hardware \
	update-nur update-kalbasit-nur

HOME_MANAGER_REF   ?= refs/heads/master
KALBASIT_NUR_REF   ?= refs/heads/master
NIXOS_HARDWARE_REF ?= refs/heads/master
NIXPKGS_REF        ?= refs/heads/nixos-unstable
NUR_REF            ?= refs/heads/master

all: build

build:
	nixos-rebuild -I nixos-config=$(shell pwd)/hosts/$(shell hostname -s)/configuration.nix build --show-trace

test:
	sudo -i nixos-rebuild -I nixos-config=$(shell pwd)/hosts/$(shell hostname -s)/configuration.nix test

# switch is not allowed to specify nixos-config as only the base repo (not any
# of the git worktrees) may switch the system
switch:
	sudo -i nixos-rebuild switch

# boot is not allowed to specify nixos-config as only the base repo (not any
# of the git worktrees) may switch the system
boot:
	sudo -i nixos-rebuild boot

brew:
	brew bundle --file=os-specific/darwin/Brewfile

add-channels:
	@echo ">>> Adding all the relevant channels, this will override any previously added channel if the version is different"
	@echo
	sudo -i nix-channel --add https://nixos.org/channels/nixos-18.09 nixos
	@echo

update-channels:
	@echo ">>> Updating all channels"
	@echo
	sudo -i nix-channel --update
	@echo

update-external: update-home-manager update-nixpkgs update-nixos-hardware update-nur update-kalbasit-nur

update-home-manager:
	nix-shell -p nix-prefetch-git --run "nix-prefetch-git https://github.com/rycee/home-manager.git $(HOME_MANAGER_REF)" > external/home-manager-version.json

update-nixpkgs:
	nix-shell -p nix-prefetch-git --run "nix-prefetch-git https://github.com/NixOS/nixpkgs-channels.git $(NIXPKGS_REF)" > external/nixpkgs-version.json

update-nixos-hardware:
	nix-shell -p nix-prefetch-git --run "nix-prefetch-git https://github.com/NixOS/nixos-hardware.git $(NIXOS_HARDWARE_REF)" > external/nixos-hardware-version.json

update-nur:
	nix-shell -p nix-prefetch-git --run "nix-prefetch-git https://github.com/nix-community/NUR.git $(NUR_REF)" > external/nur-version.json

update-kalbasit-nur:
	nix-shell -p nix-prefetch-git --run "nix-prefetch-git https://github.com/kalbasit/nur-packages.git $(KALBASIT_NUR_REF)" > external/kalbasit-nur-version.json
