{ config, pkgs, lib, ... }:

let
  cfg = config.mine.keyboard;

  layouts = {
    azerty = {
      layout = "fr";
      variant = "";
    };
    bepo = {
      layout = "fr";
      variant = "bepo";
    };
    colemak = {
      layout = "us";
      variant = "colemak";
    };
    qwerty = {
      layout = "us";
      variant = "";
    };
    qwerty_intl = {
      layout = "us";
      variant = "intl";
    };
  };
in

with lib;

{
  options = {
    mine.keyboard = {
      layouts = mkOption {
        type = types.listOf (types.enum (attrNames layouts));
        default = [ "qwerty" ];
        example = [ "colemak" "qwerty" ];
        description = "Layouts to set";
      };
    };
  };

  config = {
    # TODO: This is just colemak, add the equivalent for rest.
    system.activationScripts.postActivation = mkIf (builtins.toString (head cfg.layouts) == "colemak") {
      text = ''
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
  };
}
