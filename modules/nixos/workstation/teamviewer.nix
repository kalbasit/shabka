{ config, pkgs, lib, ... }:

with lib;

{
  options.shabka.workstation.teamviewer.enable = mkEnableOption "workstation.teamviewer";

  config = mkIf config.shabka.workstation.teamviewer.enable {
   # enable TeamViewer
   services.teamviewer.enable = true;
  };
}
