# TODO(low): all colors should be managed by themes
{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.shabka.workstation.i3.bar;

  script = ''
    set -euo pipefail

    readonly i3Socket=''${XDG_RUNTIME_DIR:-/run/user/$UID}/i3/ipc-socket.*
    if ! [ -S $i3Socket ]; then
      echo "The i3 socket is not available at $i3Socket"
      exit 1
    fi

    for m in $(${pkgs.xorg.xrandr}/bin/xrandr --query | ${pkgs.gnugrep}/bin/grep " connected" | ${pkgs.coreutils}/bin/cut -d" " -f1); do
      echo "Starting polybar on monitor $m"
      MONITOR=$m polybar -q --log=error --reload default &
    done
  '';

  batteryConstructor = {device, fullAt, ... }:
  {
    name = "module/battery-${device}";
    value = mkOrder cfg.modules.battery.order {
      type = "internal/battery";
      battery = device;
      adapter = "AC";
      full-at = fullAt;
      poll-interval = 5;
      format-charging-prefix = "⬆️";
      format-charging = "<label-charging>";
      format-charging-underline = "#ffb52a";
      label-charging = "%percentage%% %time%";
      format-discharging-prefix = "⬇️";
      format-discharging = "<label-discharging>";
      format-discharging-underline = "\${self.format-charging-underline}";
      label-discharging = "\${self.label-charging}";
      format-full-prefix = "↔️";
      format-full = "<label-full>";
      format-full-prefix-foreground = "\${colors.foreground-alt}";
      format-full-underline = "\${self.format-charging-underline}";
      label-full = "%percentage%%";
    };
  };

  timeConstructor = {format, timezone, prefix, ... }:
  {
    name = "module/time-${timezone}";
    value = mkOrder cfg.modules.time.order {
      type = "custom/script";
      exec = "TZ=${timezone} ${pkgs.coreutils}/bin/date +'${format}'";
      interval = 1;
      format-prefix = "${prefix} ";
      format-prefix-foreground = "\${colors.foreground-alt}";
      format-underline = "#0a6cf5";
    };
  };

  networkEthConstructor = interface:
  {
    name = "module/network-eth-${interface}";
    value = mkOrder cfg.modules.network.order {
      inherit interface;

      type = "internal/network";
      interval = 3;
      format-connected-underline = "#55aa55";
      format-connected-prefix = "ETH ";
      format-connected-prefix-foreground = "\${colors.foreground-alt}";
      label-connected = "%local_ip%";
      format-disconnected = "";
      format-packetloss = "<animation-packetloss> <label-connected>";
      animation-packetloss-0 = "⚠";
      animation-packetloss-0-foreground = "#ffa64c";
      animation-packetloss-1 = "📶";
      animation-packetloss-1-foreground = "#000000";
      animation-packetloss-framerate = 500;
    };
  };

  networkWlanConstructor = interface:
  {
    name = "module/network-wlan-${interface}";
    value = mkOrder cfg.modules.network.order {
      inherit interface;

      type = "internal/network";
      interval = 3;
      format-connected = "<ramp-signal> <label-connected>";
      format-connected-underline = "#9f78e1";
      label-connected = "%essid% %local_ip%";
      format-disconnected = "";
      format-packetloss = "<animation-packetloss> <label-connected>";
      ramp-signal-0 = "▁";
      ramp-signal-1 = "▂";
      ramp-signal-2 = "▃";
      ramp-signal-3 = "▅";
      ramp-signal-4 = "▆";
      ramp-signal-5 = "█";
      ramp-signal-foreground = "\${colors.foreground-alt}";
      animation-packetloss-0 = "⚠";
      animation-packetloss-0-foreground = "#ffa64c";
      animation-packetloss-1 = "📶";
      animation-packetloss-1-foreground = "#000000";
      animation-packetloss-framerate = 500;
    };
  };

  spotifyScript = pkgs.runCommand "spotify-script.py" { } ''
    install -Dm755 ${./polybar-spotify-script.py} $out
    substitute ${./polybar-spotify-script.py} $out \
      --subst-var-by python_bin ${pkgs.python3.withPackages(ps: [ ps.dbus-python ])}/bin/python
  '';

  modulesConfig =
    # Battery modules
    (optionalAttrs cfg.modules.battery.enable
      (builtins.listToAttrs (map batteryConstructor cfg.modules.battery.devices))
    ) //

    # Time modules
    (optionalAttrs cfg.modules.time.enable
      (builtins.listToAttrs (map timeConstructor cfg.modules.time.timezones))
    ) //

    # Network-eth module
    (optionalAttrs cfg.modules.network.enable
      (builtins.listToAttrs (map networkEthConstructor cfg.modules.network.eth))
    ) //

    # Network-wlan module
    (optionalAttrs cfg.modules.network.enable
      (builtins.listToAttrs (map networkWlanConstructor cfg.modules.network.wlan))
    ) //

    # Backlight module
    (optionalAttrs cfg.modules.backlight.enable {
      "module/backlight" = mkOrder cfg.modules.backlight.order {
        type = "internal/backlight";
        card = "intel_backlight";
        format = "<label> <ramp>";
        label = "%percentage%";
        # Only applies if <bar> is used
        bar-width = 10;
        bar-indicator = "|";
        bar-indicator-foreground = "#fff";
        bar-indicator-font = 2;
        bar-fill = "─";
        bar-fill-font = 2;
        bar-fill-foreground = "#9f78e1";
        bar-empty = "─";
        bar-empty-font = 2;
        bar-empty-foreground = "\${colors.foreground-alt}";
        # Only applies if <ramp> is used
        ramp-0 = "🌕";
        ramp-1 = "🌔";
        ramp-2 = "🌓";
        ramp-3 = "🌒";
        ramp-4 = "🌑";
      };
    }) //

    # Module spotify
    (optionalAttrs cfg.modules.spotify.enable {
      "module/spotify" = mkOrder cfg.modules.spotify.order {
        type = "custom/script";
        interval = 3;
        format-prefix = "";
        format = "<label>";
        exec = "${spotifyScript} -f '{play_pause} {artist} - {song}'";
        format-underline = "#1db954";
      };
    }) //

    # Module CPU
    (optionalAttrs cfg.modules.cpu.enable {
      "module/cpu" = mkOrder cfg.modules.cpu.order {
        type = "internal/cpu";
        interval = 2;
        format-prefix = "🖥️";
        format-prefix-foreground = "\${colors.foreground-alt}";
        format-underline = "#f90000";
        label = "%percentage%%";
      };
    }) //

    # Module filesystems
    (optionalAttrs cfg.modules.filesystems.enable {
      "module/filesystem" = mkOrder cfg.modules.filesystems.order {
        type = "internal/fs";
        interval = 60;
        mount-0 = (builtins.head cfg.modules.filesystems.mountPoints); # TODO: support more than one mountpoint. How to iterate over a list and increment a number in nix ?
        label-mounted = "%{F#0a81f5}%mountpoint%%{F-}: %percentage_free%%";
        label-unmounted = "%mountpoint% unmounted";
        label-unmounted-foreground = "\${colors.foreground-alt}";
      };
    }) //

    # Module RAM
    (optionalAttrs cfg.modules.ram.enable {
      "module/ram" = mkOrder cfg.modules.ram.order {
        type = "internal/memory";
        interval = 5;
        format-prefix = "💾";
        format-prefix-foreground = "\${colors.foreground-alt}";
        format-underline = "#4bffdc";
        label = "%percentage_used%%";
      };
    }) //

    # Module microphone (pulseaudio)
    (optionalAttrs cfg.modules.microphone.enable {
      "module/microphone" = mkOrder cfg.modules.microphone.order (let
        micScript = with pkgs; writeScript "mic-status.sh" ''
          #!${runtimeShell}
          set -eEuo pipefail

          readonly default_source="$(${pulseaudio}/bin/pactl info | ${gawk}/bin/awk '/Default Source/ {print $3}')"
          readonly is_muted="$(${pulseaudio}/bin/pactl list | ${gnugrep}/bin/grep -E  "Name: $default_source$|Mute:" | ${gnugrep}/bin/grep "Name:" -A1 | ${gawk}/bin/awk '/Mute:/ {print $2}')"
          readonly volume="$(${pulseaudio}/bin/pactl list | ${gnugrep}/bin/grep -E "Name: $default_source$|Volume" | ${gnugrep}/bin/grep "Name:" -A1 | ${coreutils}/bin/tail -1 | ${coreutils}/bin/cut -d% -f1 | ${coreutils}/bin/cut -d/ -f2 | ${coreutils}/bin/tr -d " ")"

          if [[ "$is_muted" == "yes" ]]; then
            echo "🔇 muted"
          elif [[ "$volume" -lt 33 ]]; then
            echo "🔈 $volume"
          elif [[ "$volume" -ge 33 ]] && [[ "$volume" -lt 66 ]]; then
            echo "🔉 $volume"
          elif [[ "$volume" -ge 66 ]]; then
            echo "🔊 $volume"
          fi
        '';
      in {
        type = "custom/script";
        interval = 3;
        exec = "${micScript}";
        format = "<label>";
        format-underline = "#1db954";
      });
    }) //

    # Module volume (pulseaudio)
    (optionalAttrs cfg.modules.volume.enable {
      "module/volume" = mkOrder cfg.modules.volume.order {
        type = "internal/pulseaudio";
        format-volume = "<ramp-volume> <label-volume> <bar-volume>";
        label-volume = "%percentage%%";
        label-volume-foreground = "\${root.foreground}";
        label-muted = "🔇 muted";
        label-muted-foreground = "#666";
        bar-volume-width = 10;
        bar-volume-foreground-0 = "#55aa55";
        bar-volume-foreground-1 = "#55aa55";
        bar-volume-foreground-2 = "#55aa55";
        bar-volume-foreground-3 = "#55aa55";
        bar-volume-foreground-4 = "#55aa55";
        bar-volume-foreground-5 = "#f5a70a";
        bar-volume-foreground-6 = "#ff5555";
        bar-volume-gradient = true;
        bar-volume-indicator = "|";
        bar-volume-indicator-font = 2;
        bar-volume-fill = "─";
        bar-volume-fill-font = 2;
        bar-volume-empty = "─";
        bar-volume-empty-font = 2;
        bar-volume-empty-foreground = "\${colors.foreground-alt}";
        ramp-volume-0 = "🔈";
        ramp-volume-1 = "🔉";
        ramp-volume-2 = "🔊";
      };
    }) //

    # Module keyboardLayout
    (optionalAttrs cfg.modules.keyboardLayout.enable {
      "module/keyboardLayout" = mkOrder cfg.modules.keyboardLayout.order {
        type = "internal/xkeyboard";
        blacklist-0 = "num lock";
        format-prefix = "";
        format-prefix-foreground = "\${colors.foreground-alt}";
        format-prefix-underline = "\${colors.secondary}";
        label-layout = "%layout%";
        label-layout-underline = "\${colors.secondary}";
        label-indicator-padding = 1;
        label-indicator-margin = 1;
        label-indicator-background = "\${colors.secondary}";
        label-indicator-underline = "\${colors.secondary}";
      };
    }) //

    # Module temperature
    (optionalAttrs cfg.modules.temperature.enable {
      "module/temperature" = mkOrder cfg.modules.temperature.order {
        type = "internal/temperature";
        # $ for i in /sys/class/thermal/thermal_zone*; do echo "$i: $(<$i/type)"; done
        thermal-zone = cfg.modules.temperature.thermalZone;
        warn-temperature = 55;
        interval = 5;
        format = "<label>";
        format-underline = "#f50a4d";
        format-warn = "<label-warn>";
        format-warn-underline = "\${self.format-underline}";
        label = "%temperature-c%";
        label-warn = "</!\> %temperature-c% </!\>";
        label-warn-foreground = "\${colors.secondary}";
      };
    });
in {
  enable = cfg.polybar.enable;
  package = pkgs.polybar.override {
    i3Support = true;
    pulseSupport = true;
  };
  inherit script;

  config = (modulesConfig // {
    "settings" = {
      pseudo-transparency = true;
      screenchange-reload = true;
    };

    "bar/default" = ({
      inherit (cfg) height;

      monitor = "\${env:MONITOR:}";
      width = "100%";
      bottom = cfg.location == "bottom";
      radius = "0.0";
      fixed-center = false;
      background = "\${colors.background}";
      foreground = "\${colors.foreground}";
      line-size = 1;
      line-color = "#f00";
      padding-left = 0;
      padding-right = 2;

      enable-ipc = true;

      # validate the font with `fc-match '<full-font-string-here>'`
      font-0 = "Source Code Pro for Powerline:style=Regular:size=9:antialias=true";
      font-1 = "xft:Twitter Color Emoji:style=Regular:size=8";

      module-margin-left = 1;
      module-margin-right = 2;

      tray-position = "right";
      tray-padding = 5;
      scroll-up = "i3wm-wsnext";
      scroll-down = "i3wm-wsprev";
      cursor-click = "pointer";
      cursor-scroll = "ns-resize";

      modules-left = "i3";
      modules-center = "";
      modules-right = (builtins.concatStringsSep " " (map (removePrefix "module/") (builtins.attrNames modulesConfig)));
    } // optionalAttrs (cfg.dpi != null) { inherit (cfg) dpi; });

    "module/i3" = {
      type = "internal/i3";
      format = "<label-state> <label-mode>";
      index-sort = true;
      wrapping-scroll = false;
      strip-wsnumbers = false;

      # Only show workspaces on the same output as the bar
      pin-workspaces = true;

      label-mode-padding = 2;
      label-mode-foreground = "#000";
      label-mode-background = "\${colors.primary}";

      # focused = Active workspace on focused monitor
      label-focused = "%name%";
      label-focused-background = "\${colors.background-alt}";
      label-focused-underline= "\${colors.primary}";
      label-focused-padding = 2;

      # unfocused = Inactive workspace on any monitor
      label-unfocused = "%name%";
      label-unfocused-padding = 1;

      # visible = Active workspace on unfocused monitor
      label-visible = "%name%";
      label-visible-background = "\${self.label-focused-background}";
      label-visible-underline = "\${self.label-focused-underline}";
      label-visible-padding = "\${self.label-focused-padding}";

      # urgent = Workspace with urgency hint set
      label-urgent = "%name%";
      label-urgent-background = "\${colors.alert}";
      label-urgent-padding = 2;

      # Separator in between workspaces
      # label-separator = "|";
    };
  });
}
