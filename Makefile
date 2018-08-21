.PHONY: all build test switch

HOSTNAME ?= $(shell hostname -s)

NIXOS_REBUILD=./lib/shell/bin/nixos-rebuild.sh

all: build

build:
	@$(NIXOS_REBUILD) $(HOSTNAME) build

test:
	@$(NIXOS_REBUILD) $(HOSTNAME) test

switch:
	@$(NIXOS_REBUILD) $(HOSTNAME) switch
