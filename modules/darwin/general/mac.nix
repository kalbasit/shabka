{ config, pkgs, lib, ... }:

let

  # TODO import what I need from this script as configurations to submit upstream
  mthsDotfiles = pkgs.fetchFromGitHub {
    owner = "mathiasbynens";
    repo = "dotfiles";
    rev = "e72d1060f3df8c157f93af52ea59508dae36ef50";
    sha256 = "1kifr4fmrzvlrk92q80snx54pnqk5x8rwp7chq5m6ylqg808mmwv";
  };

in {
  system.activationScripts.postActivation.text = ''
    readonly tmpWorkDir="$(mktemp -d)"
    trap "rm -rf ''${tmpWorkDir}" EXIT
    cp -r "${mthsDotfiles}" "''${tmpWorkDir}/dotfiles"

    pushd "''${tmpWorkDir}/dotfiles"
      ${lib.getBin pkgs.gnused}/bin/sed -e "s@open '\$HOME/init/@open '$tmpWorkDir/dotfiles/init/@g" -i .macos
      ./.macos
    popd

    # TODO: move all of these to options

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

    # Set the timezone; see `sudo systemsetup -listtimezones` for other values
    sudo systemsetup -settimezone "America/Los_Angeles" > /dev/null

    # Stop iTunes from responding to the keyboard media keys
    launchctl unload -w /System/Library/LaunchAgents/com.apple.rcd.plist 2> /dev/null

    # setup CapsLock as Control
    #
    # Use `ioreg -p IOUSB -c IOUSBDevice | grep -e class -e idVendor -e idProduct` to list the
    readonly keyboardProduct="628"
    readonly keyboardVendor="1452"
    defaults -currentHost write -g "com.apple.keyboard.modifiermapping.''${keyboardVendor}-''${keyboardProduct}-0" -array-add '
        <dict>
            <key>HIDKeyboardModifierMappingDst</key>
            <integer>30064771300</integer>
            <key>HIDKeyboardModifierMappingSrc</key>
            <integer>30064771129</integer>
        </dict>'
  '';
}
