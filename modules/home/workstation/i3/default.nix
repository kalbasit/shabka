{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.shabka.workstation.i3;
in {

  options.shabka.workstation.i3 = {
    enable = mkEnableOption "workstation.i3";

    bar = {
      engine = mkOption {
        type = types.enum [ "i3bar" "polybar" ];
        default = "i3bar";
        description = ''
          Select the bar to use with i3
        '';
      };

      battery = {
        device = mkOption {
          default = "BAT0";
          description = ''
            Battery to be monitored by the bar engine.
          '';
        };
        fullAt = mkOption {
          default = 98;
          description = ''
            In case the battery never reports 100% charge.
          '';
        };
      };

      wlan = mkOption {
        type = types.str;
        default = "";
        description = ''
          WLAN interface to be monitored by the bar engine.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = config.shabka.nixosConfig != {} && config.shabka.darwinConfig == {};
        message = "shabka.workstation.i3.enable must be false on Darwin!";
      }
      {
        assertion = (cfg.bar.engine == "polybar" && config.xsession.windowManager.i3.config.bars == []) || (cfg.bar.engine == "i3bar" && ((builtins.length config.xsession.windowManager.i3.config.bars) != 0));
        message = "There must be no i3bars if polybar is enabled.";
      }
    ];

    home.file."Desktop/.keep".text = "";

    services.polybar = import ./polybar.lib.nix { inherit config pkgs lib; };
    xdg.configFile."i3status/config" = (mkIf (cfg.bar.engine == "i3bar") {
      source = ./i3status-config;
    });

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
