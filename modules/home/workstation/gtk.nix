{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.workstation.gtk.enable = mkEnableOption "Enable GTK";

  config = mkIf config.mine.workstation.gtk.enable {
    gtk = {
      enable = true;
      font = {
        package = pkgs.hack-font;
        name = "xft:SourceCodePro:style:Regular:size=9:antialias=true";
      };
      iconTheme = {
        package = pkgs.arc-icon-theme;
        name = "Arc";
      };
      theme = {
        package = pkgs.arc-theme;
        name = "Arc-dark";
      };
    };
  };
}