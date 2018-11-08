#!/usr/bin/env bash

set -euo pipefail

readonly here="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
readonly root="$( cd "${here}/../.." && pwd )"

# Enable “natural” (Lion-style) scrolling (mths.be/macos disables it)
defaults write NSGlobalDomain com.apple.swipescrolldirection -bool true

# Set language and text formats
# Note: if you’re in the US, replace `EUR` with `USD`, `Centimeters` with
# `Inches`, `en_GB` with `en_US`, and `true` with `false`.
defaults write NSGlobalDomain AppleLanguages -array "en"
defaults write NSGlobalDomain AppleLocale -string "en_US@currency=US"
defaults write NSGlobalDomain AppleMeasurementUnits -string "Centimeters"
defaults write NSGlobalDomain AppleMetricUnits -bool true

# set the keyboard layout to Colemak
# https://github.com/hjuutilainen/dotfiles/blob/e8861a756df35cf7ade9fd964eb4f6c07ed5264b/bin/macos-system-defaults.sh#L100
sudo defaults write /Library/Preferences/com.apple.HIToolbox AppleCurrentKeyboardLayoutInputSourceID com.apple.keylayout.Colemak
sudo defaults write /Library/Preferences/com.apple.HIToolbox AppleDefaultAsciiInputSource -dict InputSourceKind "Keyboard Layout" "KeyboardLayout ID" -int 12825 "KeyboardLayout Name" Colemak
sudo defaults delete /Library/Preferences/com.apple.HIToolbox AppleEnabledInputSources
sudo defaults write /Library/Preferences/com.apple.HIToolbox AppleEnabledInputSources -array '{ InputSourceKind = "Keyboard Layout"; "KeyboardLayout ID" = 12825; "KeyboardLayout Name" = Colemak; }'
sudo defaults write /Library/Preferences/com.apple.HIToolbox AppleInputSourceHistory -array '{ InputSourceKind = "Keyboard Layout"; "KeyboardLayout ID" = 12825; "KeyboardLayout Name" = Colemak; }'
sudo defaults write /Library/Preferences/com.apple.HIToolbox AppleSelectedInputSources -array '{ InputSourceKind = "Keyboard Layout"; "KeyboardLayout ID" = 12825; "KeyboardLayout Name" = Colemak; }'
