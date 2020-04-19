{ config, pkgs, lib, ... }:

with lib;

{
  options.shabka.workstation.gtk.enable = mkEnableOption "Enable GTK";

  config = mkIf config.shabka.workstation.gtk.enable {
    services.dbus.packages = with pkgs; [ gnome3.dconf ];
  };
}

