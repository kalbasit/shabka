{ config, pkgs, lib, ... }:

with lib;
with import ../../../util;

let

  fakeExt4 = writeCLib "fake-ext4" ''
    #include <stdlib.h>
    #include <dlfcn.h>
    #include <sys/vfs.h>
    #include <linux/magic.h>

    int statfs64(const char *path, struct statfs64 *buf) {
      static int (*orig_statfs64)(const char *path, struct statfs64 *buf) = NULL;

      if (orig_statfs64 == NULL) {
        orig_statfs64 = dlsym(RTLD_NEXT, "statfs64");
      }

      int retval = orig_statfs64(path, buf);
      if (retval == 0) {
        buf->f_type = EXT4_SUPER_MAGIC;
      }
      return retval;
    }
  '';

in {
  options.mine.workstation.dropbox.enable = mkEnableOption "Enable Dropbox";

  config = mkIf config.mine.workstation.dropbox.enable {
    systemd.user.services.dropbox = {
      Environment = {
        LP_PRELOAD = fakeExt4;
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
