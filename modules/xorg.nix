{ config, pkgs, lib, ... }:

{
  services.xserver.autoRepeatDelay = 200;
  services.xserver.autoRepeatInterval = 30;
  services.xserver.xkbOptions = lib.concatStringsSep "," [
    "ctrl:nocaps"
  ];

  services.xserver.libinput.enable = true;
  services.xserver.libinput.naturalScrolling = true;

  # enable xautolock
  services.xserver.xautolock.enable = true;
  services.xserver.xautolock.locker = "${pkgs.i3lock}/bin/i3lock";
}
