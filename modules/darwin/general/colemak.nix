{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.useColemakKeyboardLayout = mkEnableOption "Use the colemak keyboard layout";

  config = mkIf config.mine.useColemakKeyboardLayout {
    system.activationScripts.postActivation.text = ''
      # set the keyboard layout to Colemak
      # https://github.com/hjuutilainen/dotfiles/blob/e8861a756df35cf7ade9fd964eb4f6c07ed5264b/bin/macos-system-defaults.sh#L100
      sudo defaults write /Library/Preferences/com.apple.HIToolbox AppleCurrentKeyboardLayoutInputSourceID com.apple.keylayout.Colemak
      sudo defaults write /Library/Preferences/com.apple.HIToolbox AppleDefaultAsciiInputSource -dict InputSourceKind "Keyboard Layout" "KeyboardLayout ID" -int 12825 "KeyboardLayout Name" Colemak
      sudo defaults delete /Library/Preferences/com.apple.HIToolbox AppleEnabledInputSources
      sudo defaults write /Library/Preferences/com.apple.HIToolbox AppleEnabledInputSources -array '{ InputSourceKind = "Keyboard Layout"; "KeyboardLayout ID" = 12825; "KeyboardLayout Name" = Colemak; }'
      sudo defaults write /Library/Preferences/com.apple.HIToolbox AppleInputSourceHistory -array '{ InputSourceKind = "Keyboard Layout"; "KeyboardLayout ID" = 12825; "KeyboardLayout Name" = Colemak; }'
      sudo defaults write /Library/Preferences/com.apple.HIToolbox AppleSelectedInputSources -array '{ InputSourceKind = "Keyboard Layout"; "KeyboardLayout ID" = 12825; "KeyboardLayout Name" = Colemak; }'
    '';
  };
}
