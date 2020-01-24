{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.shabka.workstation.i3;
in {
  options.shabka.workstation.i3.enable = mkEnableOption "workstation.i3";

  config = mkIf cfg.enable {

    home.file."Desktop/.keep".text = "";

    xsession = {
      enable = true;

      windowManager = {
        i3 = import ./i3-config.lib.nix { inherit config pkgs lib; };
      };

      initExtra = ''
        exec &> ~/.xsession-errors

        # fix the look of Java applications
        export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
      '';
    };
  };

}
