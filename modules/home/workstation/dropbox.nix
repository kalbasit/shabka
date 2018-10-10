{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.workstation.dropbox.enable = mkEnableOption "Enable Dropbox";

  config = mkIf config.mine.workstation.dropbox.enable {
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
