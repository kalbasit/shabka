{ config, pkgs, lib, ... }:

with lib;

{
  config = {
    i18n.consoleKeyMap = "colemak/en-latin9";
  } // mkIf config.mine.workstation.enable {
    services.xserver.layout = "us";
    services.xserver.xkbVariant = "colemak";
  };
}
