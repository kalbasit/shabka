{ config, lib, pkgs, ...}:

with lib;

let
  cfg = config.services.dropbox;
in {
  options = {
    services.dropbox = {
      enable = mkOption {
        default = false;
        description = ''
            Whether to enable Dropbox.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.user.services.dropbox = {
      Unit = {
        Description = "Dropbox";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Service = {
        Restart = "on-failure";
        RestartSec = 1;
        ExecStart = "${pkgs.dropbox}/bin/dropbox";
        Environment = "QT_PLUGIN_PATH=/run/current-system/sw/${pkgs.qt5.qtbase.qtPluginPrefix}";
      };

      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };
  };
}
