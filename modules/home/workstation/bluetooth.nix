{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.workstation.bluetooth.enable = mkEnableOption "workstation.bluetooth";

  config = mkIf config.mine.workstation.bluetooth.enable {
    services.blueman-applet.enable = true;
  };
}

