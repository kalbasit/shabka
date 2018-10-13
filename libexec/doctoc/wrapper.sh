#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nodejs

npx --package doctoc doctoc "${@}"
