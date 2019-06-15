{ config, pkgs, lib, ... }:

with lib;

# TODO: submit this module upstream to home-manager

let
  cfg = config.mine.less;

  colemakKeybindings = ''
    #command
    n left-scroll
    e forw-line
    i back-line
    o right-scroll
    k repeat-search
    K reverse-search
  '';

in {
  options.mine.less = {
    enable = mkEnableOption "less";

    quitIfOneScreen = mkOption {
      type = types.bool;
      default = true;
      defaultText = "true";
      description = "Causes less to automatically exit if the entire file can be displayed on the first screen.";
    };

    colors = mkOption {
      type = types.bool;
      default = true;
      defaultText = "true";
      description = "Enable colored output by allowing only ANSI color escape sequences to be written raw";
    };

    ignoreCase = mkOption {
      type = types.bool;
      default = true;
      defaultText = "true";
      description = "Search case-insensitive";
    };
  };

  config = mkIf (cfg.enable) {
    home.file.".less".source =
      let
        less-key = pkgs.writeText "less-key" (
          ''
            #env
            LESS= ${optionalString cfg.colors ''--RAW-CONTROL-CHARS''} ${optionalString cfg.quitIfOneScreen ''--no-init --quit-if-one-screen''} ${optionalString cfg.ignoreCase ''--ignore-case''}
          ''
          + optionalString ((builtins.head config.mine.keyboard.layouts) == "colemak") colemakKeybindings
        );
      in pkgs.runCommand "less-config" {
        preferLocalBuild = true;
      } ''
        ${pkgs.less}/bin/lesskey -o $out ${less-key}
      '';
  };
}
