.PHONY: all add-channels update-channels update update-nixpkgs update-nixos-hardware

all: add-channels update-channels

add-channels:
	@echo ">>> Adding all the relevant channels, this will override any previously added channel if the version is different"
	@nix-channel --add https://nixos.org/channels/nixos-18.09 nixpkgs
	@nix-channel --add https://github.com/rycee/home-manager/archive/release-18.09.tar.gz home-manager

update-channels:
	@echo ">>> Updating all channels"
	@nix-channel --update

update: update-nixpkgs update-nixos-hardware

update-nixpkgs:
	nix-shell -p nix-prefetch-git --run 'nix-prefetch-git https://github.com/NixOS/nixpkgs-channels.git refs/heads/nixos-unstable' > external/nixpkgs-version.json

update-nixos-hardware:
	nix-shell -p nix-prefetch-git --run 'nix-prefetch-git https://github.com/NixOS/nixos-hardware.git refs/heads/master' > external/nixos-hardware-version.json
