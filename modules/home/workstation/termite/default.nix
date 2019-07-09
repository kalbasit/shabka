{ config, pkgs, lib, ... }:

with lib;

{
  options.shabka.workstation.termite.enable = mkEnableOption "workstation.termite";

  config = mkIf config.shabka.workstation.termite.enable {
    programs.termite = {
      enable = true;
      font = "SourceCodePro Regular 9";
    };
  };
}
