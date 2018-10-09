{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.workstation.alacritty.enable = mkEnableOption "workstation.alacritty";

  config = mkIf config.mine.workstation.alacritty.enable {
    home.file.".config/alacritty/alacritty.yml".text = builtins.readFile ./alacritty.yml;
    home.packages = with pkgs; [ alacritty ];
  };
}
