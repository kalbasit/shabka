{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.workstation.locker.enable = mkEnableOption "Enable screen auto-locker";

  config = mkIf config.mine.workstation.locker.enable {
    services.screen-locker = {
      enable = true;
      lockCmd = "${pkgs.i3lock-fancy}/bin/i3lock-fancy -p -n -f \"Hack-Regular\" -t \"Get the fuck away\"";
      inactiveInterval = 15;
    };
  };
}
