HOME_MANAGER_REF     ?= master
KALBASIT_NUR_REF     ?= master
NIXOS_HARDWARE_REF   ?= master
NIXPKGS_UNSTABLE_REF ?= nixos-unstable
NIXPKGS_STABLE_REF   ?= nixos-18.09
NUR_REF              ?= master

.PHONY: all
all: build

.PHONY: build
build:
	./scripts/nixos-rebuild.sh "$(shell pwd)/hosts/$(shell hostname -s)/configuration.nix" build --show-trace

.PHONY: test
test:
	sudo ./scripts/nixos-rebuild.sh "$(shell pwd)/hosts/$(shell hostname -s)/configuration.nix" test --show-trace

.PHONY: switch
switch:
	sudo ./scripts/nixos-rebuild.sh "$(shell pwd)/hosts/$(shell hostname -s)/configuration.nix" switch --show-trace

.PHONY: boot
boot:
	sudo ./scripts/nixos-rebuild.sh "$(shell pwd)/hosts/$(shell hostname -s)/configuration.nix" boot --show-trace

.PHONY: brew
brew:
	brew bundle --file=os-specific/darwin/Brewfile

.PHONY: update-external
update-external: update-home-manager update-nixpkgs-stable update-nixpkgs-unstable update-nixos-hardware update-nur update-kalbasit-nur update-kalbasit-keys

.PHONY: update-home-manager
update-home-manager:
	$(eval TMP := $(shell mktemp))
	$(shell pwd)/scripts/nix-prefetch-github-url.sh rycee home-manager "$(HOME_MANAGER_REF)" > $(TMP) && mv $(TMP) external/home-manager-version.json

.PHONY: update-nixpkgs-stable
update-nixpkgs-stable:
	$(eval TMP := $(shell mktemp))
	$(shell pwd)/scripts/nix-prefetch-github-url.sh NixOS nixpkgs-channels "$(NIXPKGS_STABLE_REF)" > $(TMP) && mv $(TMP) external/nixpkgs-stable-version.json

.PHONY: update-nixpkgs-unstable
update-nixpkgs-unstable:
	$(eval TMP := $(shell mktemp))
	$(shell pwd)/scripts/nix-prefetch-github-url.sh NixOS nixpkgs-channels "$(NIXPKGS_UNSTABLE_REF)" > $(TMP) && mv $(TMP) external/nixpkgs-unstable-version.json

.PHONY: update-nixos-hardware
update-nixos-hardware:
	$(eval TMP := $(shell mktemp))
	$(shell pwd)/scripts/nix-prefetch-github-url.sh NixOS nixos-hardware "$(NIXOS_HARDWARE_REF)" > $(TMP) && mv $(TMP) external/nixos-hardware-version.json

.PHONY: update-nur
update-nur:
	$(eval TMP := $(shell mktemp))
	$(shell pwd)/scripts/nix-prefetch-github-url.sh nix-community NUR "$(NUR_REF)" > $(TMP) && mv $(TMP) external/nur-version.json

.PHONY: update-kalbasit-nur
update-kalbasit-nur:
	$(eval TMP := $(shell mktemp))
	$(shell pwd)/scripts/nix-prefetch-github-url.sh kalbasit nur-packages "$(KALBASIT_NUR_REF)" > $(TMP) && mv $(TMP) external/kalbasit-nur-version.json

.PHONY: update-kalbasit-keys
update-kalbasit-keys:
	$(eval TMP := $(shell mktemp))
	echo "{\"url\":\"https://github.com/kalbasit.keys\",\"sha256\":\"$(shell nix-prefetch-url https://github.com/kalbasit.keys)\"}" > $(TMP) && mv $(TMP) external/kalbasit-keys-version.json
