{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.useColemakKeyboardLayout = mkEnableOption "Use the colemak keyboard layout";

  config = mkIf config.mine.useColemakKeyboardLayout {
    boot.earlyVconsoleSetup = true;
    # In the following commit, the colemak/en-latin9 keyboard mapping was
    # renamed to colemak.
    # https://github.com/NixOS/nixpkgs/commit/f1987fb58f57828944ca822bbb39b3de87f01863
    i18n.consoleKeyMap = "colemak/en-latin9";
    services.xserver.layout = "us";
    services.xserver.xkbVariant = "colemak";
  };
}
