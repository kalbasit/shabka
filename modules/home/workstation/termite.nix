{ config, pkgs, lib, ... }:

with lib;

{
  options.shabka.workstation.termite.enable = mkEnableOption "workstation.termite";

  config = mkIf config.shabka.workstation.termite.enable {
    programs.termite = {
      enable = true;
      font = "Source Code Pro for Powerline 9";
    };
  };
}
