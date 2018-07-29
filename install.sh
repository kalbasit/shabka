#!/usr/bin/env bash
set -euo pipefail

readonly here="$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)"

if [[ -e "${HOME}/.config/nixpkgs" ]] && [[ ! -L "${HOME}/.config/nixpkgs" ]]; then
	echo -e "${HOME}/.config/nixpkgs already exists, cannot continue."
	exit 1
fi

if [[ -L "${HOME}/.config/nixpkgs" ]] && [[ "$(readlink -f "${HOME}/.config/nixpkgs")" != "${here}" ]]; then
	echo -e "${HOME}/.config/nixpkgs already exists as a link, but does not point here, cannot continue."
	exit 1
fi

if [[ ! -e "${HOME}/.config/nixpkgs" ]]; then
	ln -s "${here}" "${HOME}/.config/nixpkgs"
fi

echo "Installing nix profile"
home-manager switch

# TODO: migrate all these packages to the home-manager
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
