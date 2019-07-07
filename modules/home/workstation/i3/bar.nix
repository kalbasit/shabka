{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.mine.workstation.i3.bar;

  timezoneModule = types.submodule {
    options = {
      timezone = mkOption {
        type = types.str;
        default = "UTC";
        example = "Europe/Paris";
      };
      prefix = mkOption {
        type = types.str;
        default = "UTC";
        example = "FR";
      };
      format = mkOption {
        type = types.str;
        default = "%H:%M:%S";
        example = "%a %Y-%m-%d %I:%M%p";
        description = "GNU's coreutils's date format in which to display the time";
      };
    };
  };

  batteryModule = types.submodule {
    options = {
      device = mkOption {
        default = "BAT0";
        description = ''
          Battery to be monitored by the bar engine.
        '';
      };
      fullAt = mkOption {
        default = 98;
        description = ''
          In case the battery never reports 100% charge.
        '';
      };
    };
  };
in {
  options.mine.workstation.i3.bar = {
    enable = mkEnableOption "Enable workstation.i3.bar (it uses polybar)";

    location = mkOption {
      type = types.enum [ "top" "bottom" ];
      default = "top";
      description = "Location of the bar";
    };

    modules = {

      backlight.enable = mkEnableOption "Enable backlight bar module";

      battery = {
        enable = mkEnableOption "Enable battery bar module";
        devices = mkOption {
          type = types.listOf batteryModule;
          default = [ { device = "BAT0"; fullAt = 98; } ];
          description = "The battery devices to be monitored.";
        };
      };

      cpu.enable = mkEnableOption "Enable CPU bar module";

      time = {
        enable = mkEnableOption "Enable time bar module";
        timezones = mkOption {
          type = types.listOf timezoneModule;
          default = [ { timezone = "UTC"; prefix = "UTC"; format = "%H:%M:%S"; } ];
          description = "The timezones to be displayed.";
        };
      };

      filesystems = {
        enable = mkEnableOption "Enable filesystems bar module";
        mountPoints = mkOption {
          type = types.listOf types.str;
          default = [ "/" ];
          example = [ "/" "/home" ];
          description = "The mount points of which the free space will be displayed. Currently, only the first mountpoint will be displayed";
        };
      };

      ram.enable = mkEnableOption "Enable the RAM bar module";

      network = {
        enable = mkEnableOption "Enable the network bar module";
        eth = mkOption {
          type = types.listOf types.str;
          default = [ ];
          example = [ "eth0" ];
          description = "The physical network interfaces of which the status will be displayed.";
        };
        wlan = mkOption {
          type = types.listOf types.str;
          default = [ ];
          example = [ "wlp5s0" ];
          description = "The wireless network interfaces of which the status will be displayed.";
        };
      };

      volume.enable = mkEnableOption "Enable the volume bar module.";

      spotify.enable = mkEnableOption "Enable the spotify bar module.";

      keyboardLayout.enable = mkEnableOption "Display the keyboard layout in the bar";

      temperature = {
        enable = mkEnableOption "Display the temperature of the thermal zone set";
        thermalZone = mkOption {
          type = types.int;
          default = 0;
          description = ''
            The temperature of the set thermal zone will be displayed.
            You can find the number of the thermal zone by running
            `for i in /sys/class/thermal/thermal_zone*; do echo "$i: $(<$i/type)"; done`
          '';
        };
      };
    };
  };

  config = mkIf cfg.enable {
    assertions = [
    ];

    services.polybar = import ./polybar.lib.nix { inherit config pkgs lib; };
  };
}