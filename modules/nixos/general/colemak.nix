{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.useColemakKeyboardLayout = mkEnableOption "Use the colemak keyboard layout";

  config = mkIf config.mine.useColemakKeyboardLayout {
    boot.earlyVconsoleSetup = true;
    i18n.consoleKeyMap = "colemak/en-latin9";
    services.xserver.layout = "us";
    services.xserver.xkbVariant = "colemak";
  };
}
