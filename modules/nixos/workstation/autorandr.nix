{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.workstation.autorandr.enable = mkEnableOption "workstation.autorandr";

  config = mkIf config.mine.workstation.autorandr.enable {
    # This is coupled with /modules/home/workstation/autorandr.nix
    services.autorandr.enable = true;
  };
}
