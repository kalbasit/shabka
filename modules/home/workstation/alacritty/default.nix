{ config, pkgs, lib, ... }:

with lib;

let

  alacrittyConfig = builtins.readFile ./alacritty.yml;
  cfg = config.shabka.workstation.alacritty;

in {
  options.shabka.workstation.alacritty = {
    enable = mkEnableOption "workstation.alacritty";

    extraRC = mkOption {
      type = types.str;
      default = "";
      description = ''
        Extra Alacritty configuration
      '';
    };
  };

  config = mkIf cfg.enable {
    home.file.".config/alacritty/alacritty.yml".text = alacrittyConfig + cfg.extraRC;
    home.packages = with pkgs; [ alacritty ];
  };
}
