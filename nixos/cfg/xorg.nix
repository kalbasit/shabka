{ config, pkgs, lib, ... }:

{
  services.xserver.autorun = true;
  services.xserver.enable = true;
  services.xserver.autoRepeatDelay = 200;
  services.xserver.autoRepeatInterval = 30;
  services.xserver.xkbOptions = lib.concatStringsSep "," [
    "ctrl:nocaps"
  ];

  services.xserver.libinput.enable = true;
  services.xserver.libinput.naturalScrolling = true;

  # enable the display manager
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.displayManager.gdm.autoLogin = {
    enable = true;
    user = "kalbasit";
  };
}
