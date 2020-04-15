{ config, pkgs, lib, ... }:

with lib;

let
  caffeine-ng = pkgs.caffeine-ng.overrideDerivation (oldAttrs: {
    propagatedBuildInputs = with pkgs; [ procps xautolock xorg.xset ];
  });
in {
  options.shabka.workstation.locker.enable = mkEnableOption "Enable screen auto-locker";

  config = mkIf config.shabka.workstation.locker.enable {
    services.screen-locker = {
      enable = true;
      lockCmd = "${pkgs.i3lock-color}/bin/i3lock-color --clock --color=606060";
      inactiveInterval = 15;
    };

    systemd.user.services.caffeine-ng = {
      Unit = {
        Description = "Caffeine-ng, a locker inhibitor";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Install = {
        WantedBy = [ "graphical-session.target" ];
      };

      Service = {
        ExecStart = "${caffeine-ng}/bin/caffeine";
      };
    };
  };
}
