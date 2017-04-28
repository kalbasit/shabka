#!/bin/bash
set -e

# remove environments that could be from another X running
unset SESSION_MANAGER
unset DBUS_SESSION_BUS_ADDRESS

# import ZSHRC which will also export my environment
export ZSH="${ZDOTDIR:-$HOME}/.zsh"
# shellcheck disable=SC1090
source "${HOME}/.zsh/exports.zsh"
# shellcheck disable=SC1090
source "${HOME}/.zsh/functions.zsh"

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

# Use termite as the terminal
export TERMINAL=termite

# set the keyboard delay to 200ms and the repeat rate to 30Hz
xset r rate 200 30

# set beep to off
xset b off

# hide mouse cursor when it isn't used
# unclutter -root -visible &

# dynamic color temperature adjustment
redshift -l 34.42:-122.11 -m randr -t 5500:4500 -g 0.75:0.75:0.75 &

# composition manager
compton --dbus &

# notification
dunst &

# start nm-applet
# NOTE: disabled because I use nmcli
# nm-applet &

# start pulse audio
start-pulseaudio-x11

# change the resolution
if xrandr | grep -q '^DP1 connected'; then
  xrandr --output eDP1 --mode 1920x1080 --output DP1 --auto --right-of eDP1
else
  xrandr --output eDP1 --mode 1920x1080
fi

# start i3
exec dbus-launch i3 -V
