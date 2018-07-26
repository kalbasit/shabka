{ config, pkgs, lib, ... }:

let
  image = pkgs.fetchurl {
    url = "https://i.imgur.com/7kb9cEF.png"; # hosted on my imgur
    sha256 = "01k36djnp58cv1ik1afz8l2rmfwmmyizgjsysi84hf1rcjrqqqsq";
  };
in

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
  services.xserver.xautolock.locker = "${pkgs.i3lock}/bin/i3lock -ti ${image}";
}
