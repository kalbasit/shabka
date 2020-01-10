{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.shabka.tmux;

  tmuxConfig = import <shabka/modules/tmux> {
    inherit (cfg) extraRC keyboardLayout;
    inherit pkgs;
  };
in {
  options.shabka.tmux = {
    enable = mkEnableOption "Tmux";

    extraRC = mkOption {
      type = types.str;
      default = "";
      description = ''
        Extra Tmux configuration.
      '';
    };

    keyboardLayout = mkOption {
      type = with types; enum [ "colemak" "qwerty" "qwerty_intl" "bepo" "azerty" ];
      default = (builtins.head config.shabka.keyboard.layouts);
      description = ''
        The keyboard layout to use.
      '';
    };
  };

  config.programs.tmux = tmuxConfig // {
    inherit (cfg) enable;
  };
}
