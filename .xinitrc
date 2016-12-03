#!/bin/bash

# import ZSHRC which will also export my environment
# shellcheck disable=SC1090
source "${HOME}/.zshrc"

userresources="$HOME/.Xresources"
usermodmap="$HOME/.Xmodmap"
sysresources="/etc/X11/xinit/.Xresources"
sysmodmap="/etc/X11/xinit/.Xmodmap"

# merge in defaults and keymaps
[[ -f "${sysresources}" ]] && xrdb -merge "${sysresources}"
[[ -f "${sysmodmap}" ]] && xmodmap "${sysmodmap}"
[[ -f "${userresources}" ]] && xrdb -merge "${userresources}"
[[ -f "${usermodmap}" ]] && xmodmap "${usermodmap}"

# start some nice programs
if [[ -d /etc/X11/xinit/xinitrc.d ]]; then
  for f in /etc/X11/xinit/xinitrc.d/*.sh; do
    # shellcheck disable=SC1090
    [[ -x "${f}" ]] && source "${f}"
  done
  unset f
fi

# set the keyboard delay to 200ms and the repeat rate to 30Hz
xset r rate 200 30

# start i3
exec i3
