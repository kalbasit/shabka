.PHONY: all brew build test switch boot add-channels update-channels update-external update-nixpkgs update-nixos-hardware update-nur update-kalbasit-nur

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
	sudo -i nix-channel --add https://github.com/rycee/home-manager/archive/release-18.09.tar.gz home-manager
	@echo

update-channels:
	@echo ">>> Updating all channels"
	@echo
	sudo -i nix-channel --update
	@echo

update-external: update-nixpkgs update-nixos-hardware update-kalbasit-nur

update-nixpkgs:
	nix-shell -p nix-prefetch-git --run 'nix-prefetch-git https://github.com/NixOS/nixpkgs-channels.git refs/heads/nixos-unstable' > external/nixpkgs-version.json

update-nixos-hardware:
	nix-shell -p nix-prefetch-git --run 'nix-prefetch-git https://github.com/NixOS/nixos-hardware.git refs/heads/master' > external/nixos-hardware-version.json

update-nur:
	nix-shell -p nix-prefetch-git --run 'nix-prefetch-git https://github.com/nix-community/NUR.git refs/heads/master' > external/nur-version.json

update-kalbasit-nur:
	nix-shell -p nix-prefetch-git --run 'nix-prefetch-git https://github.com/kalbasit/nur-packages.git refs/heads/master' > external/kalbasit-nur-version.json
