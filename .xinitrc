#!/bin/bash
# https://wiki.archlinux.org/index.php/Xinit#Making_a_DE.2FWM_choice
set -e

# source the base
# shellcheck disable=SC1090
source "${HOME}/.xbaserc"

# source in system-wide xinit plugins
if [[ -d /etc/X11/xinit/xinitrc.d ]]; then
	for f in /etc/X11/xinit/xinitrc.d/*.sh; do
		# shellcheck disable=SC1090
		[[ -x "${f}" ]] && source "${f}"
	done
	unset f
fi

# dynamic color temperature adjustment
# TODO: fix the timing of redshift
# systemctl --user restart redshift.service

# composition manager
systemctl --user restart compton.service

# notification daemon
systemctl --user restart dunst.service

# start the xss-lock that takes care of locking the screen when necessary.
systemctl --user restart xss-lock.service

# start the wallpaper changer
systemctl --user restart gwall.service

# start time tracker
systemctl --user restart gone.service

# start xcape
systemctl --user restart xcape.service

# start the Clipboard manager
systemctl --user restart clipboard_manager.service

# start pulse audio
start-pulseaudio-x11

# start i3
exec i3
