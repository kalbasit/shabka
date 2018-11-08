#!/usr/bin/env bash

set -euo pipefail

readonly here="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
readonly root="$( cd "${here}/../.." && pwd )"

# Set the timezone; see `sudo systemsetup -listtimezones` for other values
sudo systemsetup -settimezone "America/Los_Angeles" > /dev/null

# Stop iTunes from responding to the keyboard media keys
launchctl unload -w /System/Library/LaunchAgents/com.apple.rcd.plist 2> /dev/null

# setup CapsLock as Control
#
# Use `ioreg -p IOUSB -c IOUSBDevice | grep -e class -e idVendor -e idProduct` to list the
readonly keyboardProduct="628"
readonly keyboardVendor="1452"
defaults -currentHost write -g "com.apple.keyboard.modifiermapping.${keyboardVendor}-${keyboardProduct}-0" -array-add '
    <dict>
        <key>HIDKeyboardModifierMappingDst</key>
        <integer>30064771300</integer>
        <key>HIDKeyboardModifierMappingSrc</key>
        <integer>30064771129</integer>
    </dict>'
