.PHONY: all add-channels update-channels update update-nixpkgs update-nixos-hardware update-nur update-kalbasit-nur

all: add-channels update-channels

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

update: update-nixpkgs update-nixos-hardware update-kalbasit-nur

update-nixpkgs:
	nix-shell -p nix-prefetch-git --run 'nix-prefetch-git https://github.com/NixOS/nixpkgs-channels.git refs/heads/nixos-unstable' > external/nixpkgs-version.json

update-nixos-hardware:
	nix-shell -p nix-prefetch-git --run 'nix-prefetch-git https://github.com/NixOS/nixos-hardware.git refs/heads/master' > external/nixos-hardware-version.json

update-nur:
	nix-shell -p nix-prefetch-git --run 'nix-prefetch-git https://github.com/nix-community/NUR.git refs/heads/master' > external/nur-version.json

update-kalbasit-nur:
	nix-shell -p nix-prefetch-git --run 'nix-prefetch-git https://github.com/kalbasit/nur-packages.git refs/heads/master' > external/kalbasit-nur-version.json
