{ config, pkgs, lib, ... }:

with lib;

with import <shabka/util>;

{
  options.shabka.workstation.enable = mkEnableOption "Workstation Profile";

  config = mkIf config.shabka.workstation.enable {
    shabka.workstation = enableMultiple [
      "bluetooth"
      "fonts"
      "gnome-keyring"
      "networking"
      "power"
      "redshift"
      "sound"
      "teamviewer"
      "virtualbox"
      "xorg"
    ];
  };
}
