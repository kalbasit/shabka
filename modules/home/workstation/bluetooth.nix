{ config, pkgs, lib, ... }:

with lib;

{
  options.shabka.workstation.bluetooth.enable = mkEnableOption "workstation.bluetooth";

  config = mkIf config.shabka.workstation.bluetooth.enable {
    services.blueman-applet.enable = true;
  };
}

