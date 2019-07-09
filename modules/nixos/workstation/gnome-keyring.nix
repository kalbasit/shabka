{ config, pkgs, lib, ... }:

with lib;

{
  options.shabka.workstation.gnome-keyring.enable = mkEnableOption "workstation.gnome-keyring";

  config = mkIf config.shabka.workstation.gnome-keyring.enable {
    services.gnome3.gnome-keyring.enable = true;
  };
}
