{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.mine.workstation.i3;

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
    };
  };
in {
  options.mine.workstation.i3.bar = {
    i3bar.enable = mkEnableOption "Enable workstation.i3.bar.i3bar";

    polybar.enable = mkEnableOption "Enable workstation.i3.bar.polybar";

    modules = {

      backlight.enable = mkEnableOption "Enable backlight bar module";

      battery = {
        enable = mkEnableOption "Enable battery bar module";
        devices = mkOption {
          type = types.listOf types.str;
          default = [ "BAT0̈" ];
          description = "The battery devices to be monitored by i3bar";
        };
      };

      cpu.enable = mkEnableOption "Enable CPU bar module";

      date = {
        enable = mkEnableOption "Enable date bar module";
        format = mkOption {
          type = types.str;
          default = "%Y-%m-%d";
          example = "%a %Y-%m-%d";
          description = "GNU's coreutils's date format in which to display the date";
        };
        timezone = mkOption {
          type = types.str;
          default = "UTC";
          example = "America/Los_Angeles";
          description = "The timezone of which the date will be displayed";
        };
      };

      time = {
        enable = mkEnableOption "Enable time bar module";
        format = mkOption {
          type = types.str;
          default = "%H:%M:%S";
          example = "%I:%M%p";
          description = "GNU's coreutils's date format in which to display the time";
        };
        timezones = mkOption {
          type = types.listOf timezoneModule;
          default = [ { timezone = "UTC"; prefix = "UTC"; } ];
          description = "The timezones to be displayed.";
        };
      };

      filesystems = {
        enable = mkEnableOption "Enable filesystems bar module";
        mountPoints = mkOption {
          type = types.listOf types.str;
          default = [ "/" ];
          example = [ "/" "/home" ];
          description = "The mount points of which the free space will be displayed.";
        };
      };

      ram = mkEnableOption "Enable the RAM bar module";

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
    };
  };

  config = mkIf config.mine.workstation.i3.enable {
    assertions = [
      {
        assertion = cfg.bar.i3bar.enable != cfg.bar.polybar.enable;
        message = "i3bar and polybar cannot be used at the same time.";
      }
    ];

    services.polybar = (mkIf config.mine.workstation.i3.bar.polybar.enable import ./polybar.lib.nix { inherit config pkgs lib; });
    xdg.configFile."i3status/config" = (mkIf config.mine.workstation.i3.bar.i3bar.enable import ./i3status.lib.nix { inherit config pkgs lib; });
  };
}