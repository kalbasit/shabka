{ config, pkgs, lib, ... }:

{
  # tell the console to use the Keyboard as configured in the X server
  i18n.consoleUseXkbConfig = true;

  # configure the X server Keyboard settings
  services.xserver.layout = "us";
  services.xserver.xkbVariant = "colemak";
}
