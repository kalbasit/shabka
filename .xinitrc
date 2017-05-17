#!/bin/bash
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
systemctl --user restart redshift.service

# composition manager
systemctl --user restart compton.service

# notification
dunst &

# start nm-applet
# NOTE: disabled because I use nmcli
# nm-applet &

# start pulse audio
start-pulseaudio-x11

# start the xss-lock that takes care of locking the screen when necessary.
# https://www.mankier.com/1/xss-lock
# TODO: select an image with `-i /path/to/img`
xss-lock --notifier=dim-screen -- i3lock -n &

# start i3
exec dbus-launch i3 -V
