{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.hardware.intel_backlight.enable = mkEnableOption "Enable Intel Backlight";

  config = mkIf config.mine.hardware.intel_backlight.enable {
   # Give people part of the video group access to adjust the backlight
   services.udev.extraRules = ''
     ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${pkgs.coreutils}/bin/chgrp video /sys/class/backlight/%k/brightness"
     ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/class/backlight/%k/brightness"
   '';
  };
}
