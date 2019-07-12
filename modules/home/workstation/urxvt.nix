{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.mine.workstation.urxvt;
in
{
  options.mine.workstation.urxvt.enable = mkEnableOption "workstation.urxvt";

  config = mkIf cfg.enable {

    programs.urxvt = {

      enable = true;
      package = pkgs.rxvt_unicode;

      fonts = [ "xft:SourceCodePro:style:Regular:size=9:antialias=true" ];

      keybindings = {
        "M-u" = "perl:url-select:select_next";
        "M-Escape" = "perl:keyboard-select:activate";
        "M-s" = "perl:keyboard-select:search";
        "Shift-Control-V" = "eval:paste_clipboard";
        "Shift-Control-C" = "eval:selection_to_clipboard";
        "Control-Up" = "\\033[1;5A";
        "Control-Down" = "\\033[1;5B";
        "Control-Left" = "\\033[1;5D";
        "Control-Right" = "\\033[1;5C";
        "Control-minus" = "resize-font:smaller";
        "Control-plus" = "resize-font:bigger";
        "Control-equal" = "resize-font:reset";
      };

      scroll = {
        bar.enable = false;
        keepPosition = true;
        lines = 10000;
        scrollOnKeystroke = true;
        scrollOnOutput = false;
      };

      shading = 30;
      transparent = true;

      extraConfig = {
        perl-ext-common = "default,tabbed,matcher,resize-font,-tabbed";
      };
    };
  };
}