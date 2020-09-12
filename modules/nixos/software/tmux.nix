{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.shabka.tmux;

  keyboardLayout = builtins.head config.shabka.keyboard.layouts;

  tmuxConfig = import ../../tmux {
    inherit (cfg) extraConfig;
    inherit keyboardLayout;
    inherit pkgs;
  };
in {
  options.programs.tmux.plugins = mkOption {
    default = [ ];
    type = with types; listOf package;
    description = ''
      List of tmux plugins to be included at the end of your tmux
      configuration. The sensible plugin, however, is defaulted to
      run at the top of your configuration.
    '';
  };

  options.shabka.tmux = {
    enable = mkEnableOption "Tmux";

    extraConfig = mkOption {
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
