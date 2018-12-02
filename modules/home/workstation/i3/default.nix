{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.workstation.i3.enable = mkEnableOption "workstation.i3";

  config = mkIf config.mine.workstation.i3.enable {
    assertions = [
      {
        assertion = config.mine.nixosConfig != {} && config.mine.darwinConfig == {};
        message = "mine.workstation.i3.enable must be false on Darwin!";
      }
    ];

    home.file."Desktop/.keep".text = "";
    xdg.configFile."i3status/config".source = ./i3status-config;

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
