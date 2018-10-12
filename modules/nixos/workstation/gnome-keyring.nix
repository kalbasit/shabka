{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.workstation.gnome-keyring.enable = mkEnableOption "workstation.gnome-keyring";

  config = mkIf config.mine.workstation.gnome-keyring.enable {
    services.gnome3.gnome-keyring.enable = true;
  };
}
