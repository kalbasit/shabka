{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.useColemakKeyboardLayout = mkEnableOption "Use the colemak keyboard layout";

  config = mkIf config.mine.useColemakKeyboardLayout {
    home.keyboard.layout = "us";
    home.keyboard.variant = "colemak";
  };
}
