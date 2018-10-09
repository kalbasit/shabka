{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.workstation.teamviewer.enable = mkEnableOption "workstation.teamviewer";

  config = mkIf config.mine.workstation.teamviewer.enable {
   # enable TeamViewer
   services.teamviewer.enable = true;
  };
}
