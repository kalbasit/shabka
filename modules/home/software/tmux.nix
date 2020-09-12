{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.shabka.tmux;

  tmuxConfig = import ../../tmux {
    inherit (cfg) extraConfig keyboardLayout;
    inherit pkgs;
  };
in {
  options.shabka.tmux = {
    enable = mkEnableOption "Tmux";

    extraConfig = mkOption {
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
