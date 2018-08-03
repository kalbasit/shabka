{ pkgs, ... }:

let
  defaultModifier = "Mod4";
  secondModifier = "Shift";
  thirdModifier = "Mod1";
  nosid = "--no-startup-id";
  locker = "${pkgs.xautolock}/bin/xautolock -locknow && sleep 1";

  intMonitor = "eDP-1";
    # if myHostname == "hades"
    # then "eDP-1"
    # else if myHostname == "cratos"
    # then "eDP1"
    # else "";

  intMode = "1920x1080";
    # if myHostname == "hades"
    # then "1920x1080"
    # else if myHostname == "cratos"
    # then "3200x1800"
    # else "";

  intScale = "1x1";
    # if myHostname == "hades"
    # then "1x1"
    # else if myHostname == "cratos"
    # then "0.6x0.6"
    # else "";

  extMonitor = "DP-2";
    # if myHostname == "hades"
    # then "DP-2"
    # else if myHostname == "cratos"
    # then "DP1-2"
    # else "";

  extMode = "3440x1440";
in {
  xsession.enable = true;

  xsession.windowManager.i3 = {
    enable = true;

    config = {
      fonts = [ "pango:SourceCodePro Regular 8" ];

      window = {
        commands = [
          { command = "floating enable"; criteria = { class = "^Pavucontrol"; }; }
          { command = "floating enable"; criteria = { class = "^Tor Browser"; }; }
          { command = "floating enable"; criteria = { class = "^net-filebot-Main$"; }; }
          { command = "floating enable"; criteria = { class = "^ROX-Filer$"; }; }
        ];
      };

      floating = { modifier = "${defaultModifier}"; };

      focus = {
        # focus should not follow the mouse pointer
        followMouse = false;

        # on window activation, just set the urgency hint. The default behavior is to
        # set the urgency hint if the window is not on the active workspace, and to
        # focus the window on an active workspace. It does surprise me sometimes and I
        # would like to keep it simple by having to manually switch to the urgent
        # window.
        newWindow="urgent";
      };

      assigns = {
        "slack" = [{ class = "^Slack$"; }];
        "tor" = [{ class = "^Tor Browser"; }];
        "virtualbox" = [{ class = "^VirtualBox"; }];
        "charles" = [{ class = "^com-xk72-charles-gui-.*$"; }];
      };

      modifier  = "Mod4";

      keybindings = {
        # change focus
        "${defaultModifier}+n" = "focus left";
        "${defaultModifier}+e" = "focus down";
        "${defaultModifier}+i" = "focus up";
        "${defaultModifier}+o" = "focus right";

        # move focused window
        "${defaultModifier}+${secondModifier}+n" = "move left";
        "${defaultModifier}+${secondModifier}+e" = "move down";
        "${defaultModifier}+${secondModifier}+i" = "move up";
        "${defaultModifier}+${secondModifier}+o" = "move right";

        # split in horizontal orientation
        "${defaultModifier}+h" = "split h";

        # split in vertical orientation
        "${defaultModifier}+v" = "split v";

        # change focus between output
        "${defaultModifier}+${thirdModifier}+o" = "focus output right";
        "${defaultModifier}+${thirdModifier}+n" = "focus output left";

        # move workspaces between monitors
        "${defaultModifier}+${secondModifier}+${thirdModifier}+o" = "move workspace to output right";
        "${defaultModifier}+${secondModifier}+${thirdModifier}+n" = "move workspace to output left";

        # toggle sticky
        "${defaultModifier}+s" = "sticky toggle";

        # toggle tiling / floating
        "${thirdModifier}+${secondModifier}+space" = "floating toggle";

        # change focus between tiling / floating windows
        "${thirdModifier}+space" = "focus mode_toggle";

        # enter fullscreen mode for the focused container
        "${defaultModifier}+f" = "fullscreen toggle";

        # kill focused window
        "${defaultModifier}+${secondModifier}+q" = "kill";

        # rbrowser
        "${defaultModifier}+b" = "exec ${pkgs.rbrowser}/bin/rbrowser";

        # rofi run
        "${defaultModifier}+r" = "exec ${pkgs.rofi}/bin/rofi -show run";

        # rofi lastpass
        "${defaultModifier}+p" = "exec ${pkgs.rofi}/bin/rofi -modi lpass:rofi-lpass -show lpass";

        # list open windows to switch to
        "${thirdModifier}+Tab" = "exec ${pkgs.rofi}/bin/rofi -show window";

        # switch between the current and the previously focused one
        "${defaultModifier}+Tab" = "workspace back_and_forth";
        "${defaultModifier}+${secondModifier}+Tab" = "move container to workspace back_and_forth";

        # dynamic workspaces
        "${defaultModifier}+space" = "exec ${pkgs.rofi}/bin/rofi -show i3Workspaces";
        "${defaultModifier}+${secondModifier}+space" = "exec ${pkgs.rofi}/bin/rofi -show i3MoveContainer";
        "${defaultModifier}+${thirdModifier}+space" = "exec ${pkgs.rofi}/bin/rofi -show i3RenameWorkspace";

        # change container layout (stacked, tabbed, toggle split)
        "${defaultModifier}+l" = "layout stacking";
        "${defaultModifier}+u" = "layout tabbed";
        "${defaultModifier}+y" = "layout toggle split";

        # focus the parent container
        "${defaultModifier}+a" = "focus parent";

        # focus the child container
        "${defaultModifier}+d" = "focus child";

        # start a region screenshot
        "${defaultModifier}+${secondModifier}+4" = "exec ${pkgs.maim}/bin/maim -s ~/Desktop/screenshot-`date +%Y-%m-%d_%H:%M:%S`.png";

        # focus the urgent window
        "${defaultModifier}+x" = "[urgent=latest] focus";

        # mark current window / goto mark
        # https://github.com/tybitsfox/i3msg/blob/master/.i3/config
        "${defaultModifier}+m" = "exec i3-input -F 'mark %s' -l 1 -P 'Mark: '";
        "${defaultModifier}+apostrophe" = "exec i3-input -F '[con_mark=\"%s\"] focus' -l 1 -P 'Go to: '";

        # volume support
        "XF86AudioRaiseVolume" = "exec ${nosid} ${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ false, exec ${nosid} ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ +5%";
        "XF86AudioLowerVolume" = "exec ${nosid} ${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ false, exec ${nosid} ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ -5%";
        "XF86AudioMute" = "exec ${nosid} ${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle";

        # brightness support
        "XF86MonBrightnessUp" = "exec ${nosid} ${pkgs.brightnessctl}/bin/brightnessctl s +5%";
        "XF86MonBrightnessDown" = "exec ${nosid} ${pkgs.brightnessctl}/bin/brightnessctl s 5%-";
        "${secondModifier}+XF86MonBrightnessUp" = "exec ${nosid} ${pkgs.brightnessctl}/bin/brightnessctl s +1%";
        "${secondModifier}+XF86MonBrightnessDown" = "exec ${nosid} ${pkgs.brightnessctl}/bin/brightnessctl s 1%-";

        # sleep support
        "XF86PowerOff" = "exec ${nosid} ${locker} && systemctl suspend";

        # clipboard history
        "${defaultModifier}+${thirdModifier}+c" = "exec ${pkgs.rofi}/bin/rofi -modi \"clipboard:${pkgs.haskellPackages.greenclip}/bin/greenclip print\" -show clipboard";

        # Terminals
        "${defaultModifier}+Return" = "exec ${pkgs.alacritty}/bin/alacritty";
        "${defaultModifier}+${secondModifier}+Return" = "exec ${pkgs.termite}/bin/termite";

        # Modes
        "${defaultModifier}+${thirdModifier}+r" = "mode resize";
        "${defaultModifier}+${thirdModifier}+m" = "mode move";
      };

      colors = {
        focused = {
          border = "#5f865f"; background = "#5f865f"; text = "#e4e4e4";
          indicator = "#ffafaf"; childBorder = "#285577";
        };

        focusedInactive = {
          border = "#4e4e4e"; background = "#4e4e4e"; text = "#d0d0d0";
          indicator = "#ffafaf"; childBorder = "#5f676a";
        };

        unfocused = {
          border = "#4e4e4e"; background = "#4e4e4e"; text = "#87d7d7";
          indicator = "#87af87"; childBorder = "#222222";
        };

        urgent = {
          border = "#ff0000"; background = "#ff0000"; text = "#e4e4e4";
          indicator = "#d68787"; childBorder = "#900000";
        };
      };

      modes = {
        resize = {
          # Micro resizement
          "Control+n" = "resize shrink width 10 px or 1 ppt";
          "Control+e" = "resize grow height 10 px or 1 ppt";
          "Control+i" = "resize shrink height 10 px or 1 ppt";
          "Control+o" = "resize grow width 10 px or 1 ppt";

          # Normal resizing
          "n" = "resize shrink width 50 px or 5 ppt";
          "e" = "resize grow height 50 px or 5 ppt";
          "i" = "resize shrink height 50 px or 5 ppt";
          "o" = "resize grow width 50 px or 5 ppt";

          # Macro resizing
          "${secondModifier}+n" = "resize shrink width 100 px or 10 ppt";
          "${secondModifier}+e" = "resize grow height 100 px or 10 ppt";
          "${secondModifier}+i" = "resize shrink height 100 px or 10 ppt";
          "${secondModifier}+o" = "resize grow width 100 px or 10 ppt";

          # back to normal: Enter or Escape
          "Return" = "mode default";
          "Escape" = "mode default";
        };

        move = {
          # Micro movement
          "Control+n" = "move left 10 px";
          "Control+e" = "move down 10 px";
          "Control+i" = "move up 10 px";
          "Control+o" = "move right 10 px";

          # Normal resizing
          "n" = "move left 50 px";
          "e" = "move down 50 px";
          "i" = "move up 50 px";
          "o" = "move right 50 px";

          # Macro resizing
          "${secondModifier}+n" = "move left 100 px";
          "${secondModifier}+e" = "move down 100 px";
          "${secondModifier}+i" = "move up 100 px";
          "${secondModifier}+o" = "move right 100 px";

          # back to normal: Enter or Escape
          "Return" = "mode default";
          "Escape" = "mode default";
        };
      };

      bars = [
        {
          position = "top";

          # disable clicking on workspace buttons
          # TODO: move this to the i3 module via PR
          # bindsym button1 nop

          colors = {
            background = "#626262";
            statusline = "#00ff00";
            separator = "#d68787";
            focusedWorkspace = { border = "#e4e4e4"; background = "#5f865f"; text = "#e4e4e4"; };
            activeWorkspace = { border = "#e4e4e4"; background = "#ffafaf"; text = "#e4e4e4"; };
            inactiveWorkspace = { border = "#87af87"; background = "#87af87"; text = "#626262"; };
            urgentWorkspace = { border = "#ff0000"; background = "#ff0000"; text = "#e4e4e4"; };
          };
        }
      ];
    };

    extraConfig = ''
      # keep the urgency border of a window for 500ms
      # TODO: move this to the i3 module via PR
      force_display_urgency_hint 500 ms

      # This option determines in which mode new containers on workspace level will
      # start.
      # TODO: move this to the i3 module via PR
      workspace_layout tabbed


      # Daemon launcher
      set ${defaultModifier}e_daemon Launch: (x) Xcape, (g) Greenclip
      mode "${defaultModifier}e_daemon" {
        bindsym x exec ${nosid} ${pkgs.xcape}/bin/xcape -e 'Control_L=Escape', mode "default"
        bindsym g exec ${nosid} ${pkgs.haskellPackages.greenclip}/bin/greenclip daemon, mode "default"

        # back to normal: Enter or Escape
        bindsym Return mode "default"
        bindsym Escape mode "default"
      }
      bindsym ${defaultModifier}+${thirdModifier}+l mode "${defaultModifier}e_daemon"

      # Window Manager mode, this mode allows me to control i3
      set ${defaultModifier}e_wm WM: (r) Reload i3, (e) Restart i3
      mode "${defaultModifier}e_wm" {
        bindsym r reload; exec ${nosid} ${pkgs.libnotify}/bin/notify-send 'i3 configuration reloaded', mode "default"
        bindsym e restart; exec ${nosid} ${pkgs.libnotify}/bin/notify-send 'i3 restarted', mode "default"

        # back to normal: Enter or Escape
        bindsym Return mode "default"
        bindsym Escape mode "default"
      }
      bindsym ${defaultModifier}+${thirdModifier}+w mode "${defaultModifier}e_wm"

      # Application launcher
      set ${defaultModifier}e_apps Launch: (p) Chromium personal, (u) Chromium publica, (c) Charles, (s) Slack, (i) Irc, (w) Whatsapp, (t) Teamviewer, (m) Pulse SMS
      mode "${defaultModifier}e_apps" {
        bindsym p exec ${pkgs.rbrowser}/bin/rbrowser --profile personal, mode "default"
        bindsym u exec ${pkgs.rbrowser}/bin/rbrowser --profile publica, mode "default"
        bindsym c exec ${pkgs.charles}/bin/charles, mode "default"
        bindsym s exec ${pkgs.slack}/bin/slack, mode "default"
        bindsym i exec ${pkgs.alacritty}/bin/alacritty --title=irc --exec=weechat, mode "default"
        bindsym t exec ${pkgs.teamviewer}/bin/teamviewer, mode "default"
        # TODO: install this
        # bindsym w exec whatsapp-web-desktop, mode "default"
        # TODO: install Pulse and use the binary here
        #bindsym m exec ${nosid} /opt/Pulse\ SMS/pulse-sms, mode "default"

        # back to normal: Enter or Escape
        bindsym Return mode "default"
        bindsym Escape mode "default"
      }
      bindsym ${defaultModifier}+${thirdModifier}+a mode "${defaultModifier}e_apps"

      # Display mode allows output/resolution selection
      set ${defaultModifier}e_display (l) Laptop screen, (m) Multiple screen, (w) Wide screen
      mode "${defaultModifier}e_display" {
        bindsym l exec ${nosid} ${pkgs.xlibs.xrandr}/bin/xrandr --output ${intMonitor} --mode ${intMode} --scale ${intScale} --output ${extMonitor} --off, mode "default"
        bindsym m exec ${nosid} ${pkgs.xlibs.xrandr}/bin/xrandr --output ${intMonitor} --mode ${intMode} --scale ${intScale} --output ${extMonitor} --primary --mode 3440x1440 --right-of ${intMonitor}, mode "default"
        bindsym w exec ${nosid} ${pkgs.xlibs.xrandr}/bin/xrandr --output ${intMonitor} --off --output ${extMonitor} --mode ${extMode}, mode "default"

        # back to normal: Enter or Escape
        bindsym Return mode "default"
        bindsym Escape mode "default"
      }
      bindsym ${defaultModifier}+${thirdModifier}+d mode "${defaultModifier}e_display"

      ## Management of power
      set ${defaultModifier}e_power System: (l) lock, (o) logout, (s) suspend, (h) hibernate, (r) reboot, (${secondModifier}+s) shutdown
      mode "${defaultModifier}e_power" {
        bindsym l exec ${nosid} ${locker}, mode "default"
        bindsym o exit
        bindsym s exec ${nosid} ${locker} && systemctl suspend, mode "default"
        bindsym h exec ${nosid} ${locker} && systemctl hibernate, mode "default"
        bindsym r exec ${nosid} systemctl reboot
        bindsym ${secondModifier}+s exec ${nosid} systemctl poweroff -i

        # back to normal: Enter or Escape
        bindsym Return mode "default"
        bindsym Escape mode "default"
      }
      bindsym ${defaultModifier}+${thirdModifier}+p mode "${defaultModifier}e_power"

      # CPU governor selection
      set ${defaultModifier}e_cpugovernor CPU Scaling governor: (p) Performance, (o) Powersave
      mode "${defaultModifier}e_cpugovernor" {
        bindsym p exec ${nosid} ${pkgs.gksu}/bin/gksudo -- ${pkgs.linuxPackages.cpupower}/bin/cpupower frequency-set --governor performance, mode "default"
        bindsym o exec ${nosid} ${pkgs.gksu}/bin/gksudo -- ${pkgs.linuxPackages.cpupower}/bin/cpupower frequency-set --governor powersave, mode "default"

        # back to normal: Enter or Escape
        bindsym Return mode "default"
        bindsym Escape mode "default"
      }
      bindsym ${defaultModifier}+${thirdModifier}+g mode "${defaultModifier}e_cpugovernor"
    '';
  };
}
