{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.workstation.xsession.enable = mkEnableOption "workstation.xsession";

  config = mkIf config.mine.workstation.xsession.enable {
    xsession.enable = true;

    xsession.initExtra = ''
      exec > ~/.xsession-errors 2>&1

      # fix the look of Java applications
      export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
    '' + (if config.mine.nixosConfig.networking.hostName == "cratos" then ''
      # scale by 40%
      xrandr --output eDP-1 --mode 3200x1800 --scale 0.6x0.6
    '' else "");
  };
}
