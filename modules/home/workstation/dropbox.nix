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
        ExecStart = "${getBin pkgs.dropbox}/bin/dropbox";
        ExecReload = "${getBin pkgs.coreutils}/bin/kill -HUP $MAINPID";
        KillMode = "control-group"; # upstream recommends process
        Restart = "on-failure";
        PrivateTmp = true;
        ProtectSystem = "full";
        Nice = 10;
        environment = [
          "QML2_IMPORT_PATH=/run/current-system/sw/${pkgs.qt5.qtbase.qtQmlPrefix}"
          "QT_PLUGIN_PATH=/run/current-system/sw/${pkgs.qt5.qtbase.qtPluginPrefix}"
        ];
      };

      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };
  };
}
