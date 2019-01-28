{ config, pkgs, lib, ... }:

with lib;

let

  fakeExt4 = pkgs.stdenv.mkDerivation rec {
    name = "fakext4-${version}";
    version = "0.0.1";

    src = pkgs.fetchFromGitHub {
      owner = "dark";
      repo = "dropbox-filesystem-fix";
      rev = "d284f5d4884003c2dad24eac88b5e285f6281624";
      sha256 = "0a5gfb11nb26lpavppyfifklnw515sg402sy9cqm8h39gw2zkb87";
    };

    buildInputs = [ pkgs.makeWrapper ];

    installPhase = ''
      install -Dm644 libdropbox_fs_fix.so $out/lib/libdropbox_fs_fix.so
    '';
  };

in {
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
          "LD_PRELOAD=${getLib fakeExt4}/lib/libdropbox_fs_fix.so"
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
