{ config, pkgs, lib, ... }:

with lib;

with import ../../../util;

{
  options.mine.workstation.enable = mkEnableOption "Workstation Profile";

  config = mkIf config.mine.workstation.enable {
    mine.workstation = enableMultiple [
      "autorandr"
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
