{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.workstation.power.enable = mkEnableOption "workstation.power";

  config = mkIf config.mine.workstation.power.enable {
    powerManagement = {
      enable = true;
      cpuFreqGovernor = null;
    };
  };
}

