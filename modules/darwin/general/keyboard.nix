{ config, pkgs, lib, ... }:

let
  cfg = config.shabka.keyboard;

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

  # Logic copied from
  # https://github.com/hjuutilainen/dotfiles/blob/e8861a756df35cf7ade9fd964eb4f6c07ed5264b/bin/macos-system-defaults.sh#L100
  setLayout = id: name: ''
    sudo defaults write /Library/Preferences/com.apple.HIToolbox AppleCurrentKeyboardLayoutInputSourceID com.apple.keylayout.${name}
    sudo defaults write /Library/Preferences/com.apple.HIToolbox AppleDefaultAsciiInputSource -dict InputSourceKind "Keyboard Layout" "KeyboardLayout ID" -int ${builtins.toString id} "KeyboardLayout Name" ${name}
    sudo defaults delete /Library/Preferences/com.apple.HIToolbox AppleEnabledInputSources
    sudo defaults write /Library/Preferences/com.apple.HIToolbox AppleEnabledInputSources -array '{ InputSourceKind = "Keyboard Layout"; "KeyboardLayout ID" = ${builtins.toString id}; "KeyboardLayout Name" = ${name}; }'
    sudo defaults write /Library/Preferences/com.apple.HIToolbox AppleInputSourceHistory -array '{ InputSourceKind = "Keyboard Layout"; "KeyboardLayout ID" = ${builtins.toString id}; "KeyboardLayout Name" = ${name}; }'
    sudo defaults write /Library/Preferences/com.apple.HIToolbox AppleSelectedInputSources -array '{ InputSourceKind = "Keyboard Layout"; "KeyboardLayout ID" = ${builtins.toString id}; "KeyboardLayout Name" = ${name}; }'
  '';
in

with lib;

{
  options = {
    shabka.keyboard = {
      layouts = mkOption {
        type = types.listOf (types.enum (attrNames layouts));
        default = [ "qwerty" ];
        example = [ "colemak" "qwerty" ];
        description = "Layouts to set";
      };
    };
  };

  config = {
    system.activationScripts.postActivation = mkIf ((builtins.head cfg.layouts) == "qwerty") {
      text = setLayout 1 "US";
    };

    system.activationScripts.postActivation = mkIf ((builtins.head cfg.layouts) == "colemak") {
      text = setLayout 12825 "Colemak";
    };
  };
}
