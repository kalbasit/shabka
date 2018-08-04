{ config, pkgs, lib, ... }:

let
  image = pkgs.fetchurl {
    url = "https://i.imgur.com/VwqWOwM.png"; # hosted on my imgur
    sha256 = "0mw3nk4aqjdikm4q9f0f65kj99w4sv93r0naf8s8366h5sdvw5g3";
  };
in

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

  # enable xautolock
  services.xserver.xautolock.enable = true;
  services.xserver.xautolock.locker = "${pkgs.i3lock}/bin/i3lock -ti ${image}";

  # enable the display manager
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.displayManager.gdm.autoLogin = {
    enable = true;
    user = "kalbasit";
  };
}
