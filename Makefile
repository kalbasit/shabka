HOME_MANAGER_REF     ?= master
KALBASIT_NUR_REF     ?= master
NIXOS_HARDWARE_REF   ?= master
NIXPKGS_STABLE_REF   ?= nixos-19.03
NIXPKGS_UNSTABLE_REF ?= nixos-unstable
NIX_DARWIN_REF       ?= master
NUR_REF              ?= master

.PHONY: all
all: build

.PHONY: build
build:
	./scripts/nixos-rebuild.sh build --show-trace

.PHONY: test
test:
	sudo ./scripts/nixos-rebuild.sh test --show-trace

.PHONY: switch
switch:
	sudo ./scripts/nixos-rebuild.sh switch --show-trace

.PHONY: boot
boot:
	sudo ./scripts/nixos-rebuild.sh boot --show-trace

.PHONY: brew
brew:
	brew bundle --file=os-specific/darwin/Brewfile

.PHONY: update-external
update-external: update-home-manager update-nixpkgs-stable update-nixpkgs-unstable update-nixos-hardware update-nur update-kalbasit-nur update-kalbasit-keys

.PHONY: update-home-manager
update-home-manager:
	$(shell pwd)/scripts/update-external.sh rycee home-manager "$(HOME_MANAGER_REF)" external/home-manager-version.json

.PHONY: update-nix-darwin
update-nix-darwin:
	$(shell pwd)/scripts/update-external.sh LnL7 nix-darwin "$(NIX_DARWIN_REF)" external/nix-darwin-version.json

.PHONY: update-nixpkgs-stable
update-nixpkgs-stable:
	$(shell pwd)/scripts/update-external.sh NixOS nixpkgs-channels "$(NIXPKGS_STABLE_REF)" external/nixpkgs-stable-version.json

.PHONY: update-nixpkgs-unstable
update-nixpkgs-unstable:
	$(shell pwd)/scripts/update-external.sh NixOS nixpkgs-channels "$(NIXPKGS_UNSTABLE_REF)" external/nixpkgs-unstable-version.json

.PHONY: update-nixos-hardware
update-nixos-hardware:
	$(shell pwd)/scripts/update-external.sh NixOS nixos-hardware "$(NIXOS_HARDWARE_REF)" external/nixos-hardware-version.json

.PHONY: update-nur
update-nur:
	$(shell pwd)/scripts/update-external.sh nix-community NUR "$(NUR_REF)" external/nur-version.json

.PHONY: update-kalbasit-nur
update-kalbasit-nur:
	$(shell pwd)/scripts/update-external.sh kalbasit nur-packages "$(KALBASIT_NUR_REF)" external/kalbasit-nur-version.json

.PHONY: update-kalbasit-keys
update-kalbasit-keys:
	$(eval TMP := $(shell mktemp))
	echo "{\"url\":\"https://github.com/kalbasit.keys\",\"sha256\":\"$(shell nix-prefetch-url https://github.com/kalbasit.keys)\"}" > $(TMP) && mv $(TMP) external/kalbasit-keys-version.json
