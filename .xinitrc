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

# notification daemon
systemctl --user restart dunst.service

# start the xss-lock that takes care of locking the screen when necessary.
systemctl --user restart xss-lock.service

# start pulse audio
start-pulseaudio-x11

# start i3
exec i3 -V
