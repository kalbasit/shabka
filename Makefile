.PHONY: all
all: build

.PHONY: build
build:
	./scripts/nixos-rebuild.sh build --show-trace

.PHONY: test
test:
	sudo --preserve-env=DOTSHABKA_PATH ./scripts/nixos-rebuild.sh test --show-trace

.PHONY: switch
switch:
	sudo --preserve-env=DOTSHABKA_PATH ./scripts/nixos-rebuild.sh switch --show-trace

.PHONY: boot
boot:
	sudo --preserve-env=DOTSHABKA_PATH ./scripts/nixos-rebuild.sh boot --show-trace

.PHONY: brew
brew:
	brew bundle --file=os-specific/darwin/Brewfile
