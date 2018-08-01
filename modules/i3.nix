{ config, pkgs, lib, ... }:

{
  imports = [
    ./xorg.nix
  ];

  services.xserver.autorun = true;
  services.xserver.enable = true;

  services.xserver.windowManager.i3.enable = true;
}
