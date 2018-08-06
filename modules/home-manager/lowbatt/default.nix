# TODO: see https://github.com/rycee/home-manager/issues/250 for instructions

{ config, lib, pkgs, ...}:

with lib;

let
  cfg = config.services.batteryNotifier;
  script = pkgs.writeTextFile {
    name = "unit-script";
    executable = true;
    destination = "/bin/lowbatt-start";
    text = ''
        export battery_capacity=$(${pkgs.coreutils}/bin/cat /sys/class/power_supply/${cfg.device}/capacity)
        export battery_status=$(${pkgs.coreutils}/bin/cat /sys/class/power_supply/${cfg.device}/status)

        if [[ $battery_capacity -le ${builtins.toString cfg.notifyCapacity} && $battery_status = "Discharging" ]]; then
            ${pkgs.libnotify}/bin/notify-send --urgency=critical --hint=int:transient:1 --icon=battery_empty "Battery Low" "You should probably plug-in."
        fi

        if [[ $battery_capacity -le ${builtins.toString cfg.hibernateCapacity} && $battery_status = "Discharging" ]]; then
            ${pkgs.libnotify}/bin/notify-send --urgency=critical --hint=int:transient:1 --icon=battery_empty "Battery Critically Low" "Computer will hibernate in 60 seconds."
            sleep 60s

            battery_status=$(${pkgs.coreutils}/bin/cat /sys/class/power_supply/${cfg.device}/status)
            if [[ $battery_status = "Discharging" ]]; then
                systemctl hibernate
            fi
        fi
    '';
  };

in {
  options = {
    services.batteryNotifier = {
      enable = mkOption {
        default = false;
        description = ''
          Whether to enable battery notifier.
        '';
      };
      device = mkOption {
        default = "BAT0";
        description = ''
          Device to monitor.
        '';
      };
      notifyCapacity = mkOption {
        default = 10;
        description = ''
          Battery level at which a notification shall be sent.
        '';
      };
      hibernateCapacity = mkOption {
        default = 5;
        description = ''
          Battery level at which a hibernate unless connected shall be sent.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.user.timers."lowbatt" = {
      Unit = {
        Description = "check battery level";
      };

      Timer = {
        OnBootSec = "1m";
        OnUnitInactiveSec = "1m";
        Unit = "lowbatt.service";
      };

      Install = {
        WantedBy = [ "timers.target" ];
      };
    };

    systemd.user.services."lowbatt" = {
      Unit = {
        Description = "battery level notifier";
      };

      Service = {
        PassEnvironment = "DISPLAY";
        ExecStart = "${script}/bin/lowbatt-start";
      };
    };
  };
}
