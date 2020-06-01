{ config, lib, pkgs, ... }:

with lib;

{
  options.shabka.workstation.bluetooth.enable = mkEnableOption "workstation.bluetooth";

  config = mkIf config.shabka.workstation.bluetooth.enable {
    hardware.bluetooth.enable = true;
    services.blueman.enable = true;
  };
}
