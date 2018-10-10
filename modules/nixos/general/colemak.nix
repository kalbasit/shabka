{ config, pkgs, lib, ... }:

with lib;

{
  config = {
    boot.earlyVconsoleSetup = true;
    i18n.consoleKeyMap = "colemak/en-latin9";
    services.xserver.layout = "us";
    services.xserver.xkbVariant = "colemak";
  };
}
