{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.shabka.tmux;

  keyboardLayout = builtins.head config.shabka.keyboard.layouts;

  tmuxConfig = import <shabka/modules/tmux> {
    inherit (cfg) extraRC;
    inherit keyboardLayout;
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
  };

  config.programs.tmux = tmuxConfig // {
    inherit (cfg) enable;
  };
}
