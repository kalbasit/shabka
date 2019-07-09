{ config, pkgs, lib, ... }:

with lib;

{
  options.shabka.workstation.power.enable = mkEnableOption "workstation.power";

  config = mkIf config.shabka.workstation.power.enable {
    powerManagement = {
      enable = true;
    };
  };
}
