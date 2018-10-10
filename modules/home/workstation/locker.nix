{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.workstation.locker.enable = mkEnableOption "Enable screen auto-locker";

  config = mkIf config.mine.workstation.locker.enable {
    services.screen-locker = {
      enable = true;
      lockCmd = "${pkgs.i3lock-color}/bin/i3lock-color --clock --color=606060";
      inactiveInterval = 15;
    };
  };
}
