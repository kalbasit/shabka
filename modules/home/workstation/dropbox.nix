{ config, pkgs, lib, ... }:

with lib;

let

  fakeExt4 = pkgs.stdenv.mkDerivation rec {
    name = "fakext4-${version}";
    version = "0.0.1";

    src = pkgs.fetchFromGitHub {
      owner = "dimaryaz";
      repo = "dropbox_ext4";
      rev = "7cb936588ddd5992fb2c2c8f19b6015cf607a4f5";
      sha256 = "18rhjxar46qm38j6cw22xlqrbbl1gvlhrqdpkyd6h3a9lzrplp3a";
    };

    makeFlags = [ "INSTALL_DIR=$(out)" ];

    preInstall = ''
      mkdir -p $out/lib
      mkdir -p $out/bin
    '';

    postInstall = ''
      rm -rf $out/bin
    '';
  };

in {
  options.mine.workstation.dropbox.enable = mkEnableOption "Enable Dropbox";

  config = mkIf config.mine.workstation.dropbox.enable {
    systemd.user.services.dropbox = {
      Environment = {
        LP_PRELOAD = "${getLib fakeExt4}/lib/libdropbox_ext4.so";
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
