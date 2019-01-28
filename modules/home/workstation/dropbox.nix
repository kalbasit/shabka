{ config, pkgs, lib, ... }:

with lib;
with pkgs;

let

  fakeExt4 = stdenv.mkDerivation rec {
    name = "fakext4-${version}";
    version = "0.0.1";

    src = fetchFromGitHub {
      owner = "dimaryaz";
      repo = "dropbox_ext4";
      ref = "7cb936588ddd5992fb2c2c8f19b6015cf607a4f5";
      sha256 = "0000000000000000000000000000000000000000000000000000";
    };
  };

in {
  options.mine.workstation.dropbox.enable = mkEnableOption "Enable Dropbox";

  config = mkIf config.mine.workstation.dropbox.enable {
    systemd.user.services.dropbox = {
      Environment = {
        LP_PRELOAD = "${getLib fakeExt4}/lib";
      };

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
