{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.workstation.networking.enable = mkEnableOption "workstation.networking";

  config = mkIf config.mine.workstation.networking.enable {
    networking.networkmanager.enable = true;
  };
}

