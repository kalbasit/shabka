{ config, pkgs, lib, ... }:

with lib;

{
  options.shabka.workstation.networking.enable = mkEnableOption "workstation.networking";

  config = mkIf config.shabka.workstation.networking.enable {
    networking.networkmanager = {
      enable = true;
      dns = "dnsmasq";
    };

    shabka.users.groups = ["networkmanager"];
  };
}

