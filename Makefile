.PHONY: all
all: build

.PHONY: build
build:
	./bin/shabka build --show-trace

.PHONY: test
test:
	sudo --preserve-env=DOTSHABKA_PATH ./bin/shabka test --show-trace

.PHONY: switch
switch:
	sudo --preserve-env=DOTSHABKA_PATH ./bin/shabka switch --show-trace

.PHONY: boot
boot:
	sudo --preserve-env=DOTSHABKA_PATH ./bin/shabka boot --show-trace

.PHONY: brew
brew:
	brew bundle --file=os-specific/darwin/Brewfile
