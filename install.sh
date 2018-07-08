#!/usr/bin/env bash
set -euo pipefail

echo "Installing nix profile"
nix-env -i all

if [[ -d "${HOME}/.nix-profile/userHome" ]]; then
	echo "Creating dotfiles links in user home"
	pushd "${HOME}" > /dev/null
	for f in $( find -L .nix-profile/userHome -type f | sed 's#.nix-profile/userHome/##g' ); do
		mkdir -p "${HOME}/$( dirname "${f}" )"
		ln -sf "${HOME}/.nix-profile/userHome/${f}" "${HOME}/${f}"
	done
	popd > /dev/null
fi

if [[ -f /etc/arch-release ]]; then
	./arch.sh
fi

echo "Done"
