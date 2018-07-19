#!/usr/bin/env nix-shell
#! nix-shell -i bash -p go2nix go git

readonly here="$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)"
export GOPATH="$(mktemp -d /tmp/gopath.XXXXXX)"

go get -u github.com/knqyf263/pet

cd "${GOPATH}/src/github.com/knqyf263/pet"

go2nix save -o "${here}/go-package.nix" -d "${here}/deps.nix"

# clean up
cd "${here}"
rm -rf "${GOPATH}"
