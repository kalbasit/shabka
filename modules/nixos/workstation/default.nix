{ config, pkgs, lib, ... }:

with lib;

with import ../../../util;

{
  options.mine.workstation.enable = mkEnableOption "Workstation Profile";

  config = mkIf config.mine.workstation.enable {
    # set the BROWSER to my rbrowser
    # TODO: move this to the home
    environment.variables.BROWSER = "${pkgs.rbrowser}/bin/rbrowser";

    services.xserver = {
      autorun = true;
      enable = true;
      autoRepeatDelay = 200;
      autoRepeatInterval = 30;
      xkbOptions = lib.concatStringsSep "," [
        "ctrl:nocaps"
        ];

      libinput.enable = true;
      libinput.naturalScrolling = true;

      # enable the display manager
      displayManager.gdm.enable = true;
      displayManager.gdm.autoLogin = {
        enable = true;
        user = "kalbasit";
      };
    };

    mine.workstation = enableMultiple [
      "fonts" "networking" "power" "sound" "gnupg" "redshift"
    ];
  };
}
