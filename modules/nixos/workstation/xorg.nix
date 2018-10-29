{ config, pkgs, lib, ... }:

with lib;

let

  cfg = config.mine.workstation.xorg;

in {
  options.mine.workstation.xorg.enable = mkEnableOption "Enable the Xorg server";

  config = mkIf cfg.enable {
    # set the BROWSER to my rbrowser
    # TODO: move this to the home
    environment.variables.BROWSER = "${pkgs.nur.repos.kalbasit.rbrowser}/bin/rbrowser";

    # start the autorandr service
    services.autorandr.enable = true;

    services.xserver = {
      autorun = true;
      enable = true;
      autoRepeatDelay = 200;
      autoRepeatInterval = 30;
      xkbOptions = concatStringsSep "," [
        "ctrl:nocaps"
      ];

      libinput.enable = true;
      libinput.naturalScrolling = true;

      desktopManager.gnome3.enable = true;
      desktopManager.plasma5.enable = true;

      # enable the display manager
      displayManager.lightdm.enable = true;
      displayManager.lightdm.autoLogin = {
        enable = true;
        user = "yl";
      };
    };
  };
}
