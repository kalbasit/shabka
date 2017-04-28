#!/bin/bash
set -e

# source the base
# shellcheck disable=SC1090
source "${HOME}/.xbaserc"

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

# start i3
exec dbus-launch i3 -V
