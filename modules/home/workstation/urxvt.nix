{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.shabka.workstation.urxvt;
in
{
  options.shabka.workstation.urxvt = {
    enable = mkEnableOption "workstation.urxvt";

    transparency = mkEnableOption "Enable transparency on workstation.urxvt.";
    scrollOnOutput = mkEnableOption "workstation.urxvt Scroll on output";
  };

  config = mkIf cfg.enable {

    programs.urxvt = {

      enable = true;
      package = pkgs.rxvt_unicode-with-plugins;

      fonts = [ "xft:SourceCodePro:style:Regular:size=9:antialias=true" ];

      keybindings = {
        "Shift-Control-V" = "eval:paste_clipboard";
        "Shift-Control-C" = "eval:selection_to_clipboard";
        "Control-Up" = "\\033[1;5A";
        "Control-Down" = "\\033[1;5B";
        "Control-Left" = "\\033[1;5D";
        "Control-Right" = "\\033[1;5C";
      };

      scroll = {
        bar.enable = false;
        keepPosition = true;
        lines = 10000;
        scrollOnKeystroke = true;
        scrollOnOutput = cfg.scrollOnOutput;
      };

      shading = if cfg.transparency then 30 else 100;
      transparent = cfg.transparency;

      extraConfig = {
        perl-ext-common = "default";
      };
    };
  };
}