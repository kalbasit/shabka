{ config, pkgs, lib, ... }:

with lib;

{
  options.shabka.workstation.autorandr.enable = mkEnableOption "workstation.autorandr";

  config = mkIf config.shabka.workstation.autorandr.enable {
    # This is coupled with /modules/home/workstation/autorandr.nix
    services.autorandr.enable = true;
  };
}
