{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.workstation.termite.enable = mkEnableOption "workstation.termite";

  config = mkIf config.mine.workstation.termite.enable {
    programs.termite = {
      enable = true;
      font = "SourceCodePro Regular 9";
    };
  };
}
