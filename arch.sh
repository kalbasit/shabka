#!/usr/bin/env bash
set -euo pipefail

# make sure pacman is colorful
if grep -q '^#Color' /etc/pacman.conf; then
	sudo sed -e 's:^#Color:Color:g' -i /etc/pacman.conf
fi

# NOTE: This is needed because Termite could not find the xterm-termite terminfo
# See https://github.com/NixOS/nixpkgs/issues/19785
if [[ ! -L "${HOME}/.terminfo" ]]; then
	echo "Linking terminfo to allow terminals to find installed terminfo"
	ln -sf .nix-profile/share/terminfo "${HOME}/.terminfo"
fi

while read line; do
	if [[ "$(pacman -Qi "${line}" | wc -l)" -eq 0 ]]; then
		echo "installing ${line} in the OS"
		yay -S --nocleanmenu --nodiffmenu --noeditmenu --noupgrademenu --noconfirm "${line}"
	fi
done < <(grep -v '^#\|^$' arch-packages)

if [[ ! -f /etc/ca-certificates/trust-source/anchors/nasreddine.crt ]]; then
	curl -LO http://nasreddine.com/ca.crt
	sudo mv ca.crt /etc/ca-certificates/trust-source/anchors/nasreddine.crt
	sudo trust extract-compat
fi
