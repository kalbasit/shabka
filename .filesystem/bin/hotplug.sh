#!/bin/bash

bootstrap="$(dirname $0)/.bootstrap"
if [ -f "${bootstrap}" ]; then
    source "${bootstrap}"
else
    echo "Cannot bootstrap"
    exit 54
fi
unset bootstrap

ConnectToX

DP1_STATUS_FILE="/sys/class/drm/card0-DP-1/status"
DP1_STATUS="$(cat $DP1_STATUS_FILE)"

case "${DP1_STATUS}" in
  disconnected)
    xrandr --output DP1 --off ;;
  connected)
    xrandr --output eDP1 --mode 1920x1080 --output DP1 --mode 2560x1600 --left-of eDP1 ;;
esac
