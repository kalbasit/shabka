{ config, lib, pkgs, ... }:

{
      "settings" = {
        pseudo-transparency = true;
        screenchange-reload = true;
      };

      "colors" = {
        background = "#222";
        background-alt = "#444";
        foreground = "#dfdfdf";
        foreground-alt = "#555";
        primary = "#ffb52a";
        secondary = "#e60053";
        alert = "#bd2c40";
      };

      "bar/bottom" = {
        #monitor = "\${env:MONITOR:}";

        width = "100%";
        height = 21;
        bottom = true;
        radius = "0.0";
        fixed-center = false;
        background = "\${colors.background}";
        foreground = "\${colors.foreground}";
        line-size = 1;
        line-color = "#f00";
        padding-left = 0;
        padding-right = 2;

        enable-ipc = true;

        module-margin-left = 1;
        module-margin-right = 2;
        font-0 = "Hack Regular :size=10:antialias=false";
        font-1 = "Twitter Color Emoji:size=10";

        tray-position = "right";
        tray-padding = 5;
        scroll-up = "i3wm-wsnext";
        scroll-down = "i3wm-wsprev";
        cursor-click = "pointer";
        cursor-scroll = "ns-resize";

        modules-left = "i3";
        modules-center = "";
#        modules-right = "spotify pulseaudio info-pingrtt network-traffic network-eth network-wlan info-hackspeed cpu memory backlight battery date-california date-utc popup-calendar xkeyboard";#modules-right = "spotify pulseaudio info-pingrtt network-traffic network-eth network-wlan info-hackspeed updates-arch-combined cpu memory xbacklight battery date-california date-utc xkeyboard";
        modules-right = "pulseaudio network-eth network-wlan cpu memory backlight battery date-california date-utc xkeyboard";
      };

      "module/backlight" = {
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

      "module/battery" = {
        type = "internal/battery";
        battery = "BAT0";
        adapter = "AC";
        full-at = 98;
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

      "module/cpu" = {
        type = "internal/cpu";
        interval = 2;
        format-prefix = "🖥️";
        format-prefix-foreground = "\${colors.foreground-alt}";
        format-underline = "#f90000";
        label = "%percentage%%";
      };

      "module/date-utc" = {
        type = "custom/script";
        exec = "TZ=UTC date +\"%H:%M:%S\"";
        interval = 1;
        format-prefix = "UTC ";
        format-prefix-foreground = "\${colors.foreground-alt}";
        format-underline = "#0a6cf5";
      };

      "module/date-california" = {
        type = "custom/script";
        exec = "TZ=America/Los_Angeles date +\"%a %Y-%m-%d %H:%M:%S\"";
        interval = 1;
        format-prefix = "SF ";
        format-prefix-foreground = "\${colors.foreground-alt}";
        format-underline = "#0a6cf5";
      };

      "module/filesystem" = {
        type = "internal/fs";
        interval = 60;

        mount-0 = "/";
        mount-1 = "/home";

        label-mounted = "%{F#0a81f5}%mountpoint%%{F-}: %percentage_free%%";
        label-unmounted = "%mountpoint% unmounted";
        label-unmounted-foreground = "\${colors.foreground-alt}";
      };

      "module/i3" = {
        type = "internal/i3";
        format = "<label-state> <label-mode>";
        index-sort = true;
        wrapping-scroll = false;
        strip-wsnumbers = true;

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
        # label-separator = |
      };

      "module/info-hackspeed" = {
        type = "custom/script";
        exec = "$HOME/.config/nixpkgs/polybar/info-hackspeed.sh";
        tail = true;
      };

      "module/info-pingrtt" = {
        type = "custom/script";
        exec = "$HOME/.config/nixpkgs/polybar/info-pingrtt.sh";
        interval = 2;
      };

      "module/memory" = {
        type = "internal/memory";
        interval = 5;
        format-prefix = "💾";
        format-prefix-foreground = "\${colors.foreground-alt}";
        format-underline = "#4bffdc";
        label = "%percentage_used%%";
      };

      "module/network-eth" = {
        type = "internal/network";
        interface = "enp3s0";
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

      "module/network-wlan" = {
        type = "internal/network";
        interface = "wlp5s0";
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

      "module/network-traffic" = {
        type = "custom/script";
        exec = "$HOME/.config/nixpkgs/polybar/network-traffic.sh";
        tail = true;
      };

      "module/popup-calendar" = {
        type = "custom/script";
        exec = "$HOME/.config/nixpkgs/polybar/popup-calendar.sh";
        interval = 5;
        click-left = "HOME/.config/nixpkgs/polybar/popup-calendar.sh --popup";
      };

      "module/pulseaudio" = {
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

      "module/spotify" = {
        type = "custom/script";
        interval = 3;
        format-prefix = "";
        format = "<label>";
        exec = "$HOME/.config/nixpkgs/polybar/spotify-status.py -f '{play_pause} {artist} - {song}'";
        format-underline = "#1db954";
      };

      "module/system-uptime-pretty" = {
        type = "custom/script";
        exec = "$HOME/.config/nixpkgs/polybar/system-uptime-pretty.sh";
        interval = 600;
      };

      "module/temperature" = {
        type = "internal/temperature";
        # $ for i in /sys/class/thermal/thermal_zone*; do echo "$i: $(<$i/type)"; done
        thermal-zone = 1;
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

      "module/xkeyboard" = {
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

      "module/powermenu" = {
        type = "custom/menu";

        expand-right = true;

        format-spacing = 1;

        label-open = "open";
        label-open-foreground = "\${colors.secondary}";
        label-close = "cancel";
        label-close-foreground = "\${colors.secondary}";
        label-separator = "|";
        label-separator-foreground = "\${colors.foreground-alt}";

        menu-0-0 = "reboot";
        menu-0-0-exec = "menu-open-1";
        menu-0-1 = "power off";
        menu-0-1-exec = "menu-open-2";

        menu-1-0 = "cancel";
        menu-1-0-exec = "menu-open-0";
        menu-1-1 = "reboot";
        menu-1-1-exec = "sudo reboot";

        menu-2-0 = "power off";
        menu-2-0-exec = "sudo poweroff";
        menu-2-1 = "cancel";
        menu-2-1-exec = "menu-open-0";
      };

      "global/wm" = {
        margin-top = 5;
        margin-bottom = 5;
      };
}