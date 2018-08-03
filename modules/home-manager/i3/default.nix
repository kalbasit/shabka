{ pkgs, ... }:

{
  xsession.enable = true;

  xsession.windowManager.i3 = {
    enable = true;

    config = {
      fonts = [ "pango:SourceCodePro Regular 8" ];

      window = {
        commands = [
          {
            command = "floating enable";
            criteria = {
              class = "^Pavucontrol";
            };
          }
          {
            command = "floating enable";
            criteria = {
              class = "^Tor Browser";
            };
          }
          {
            command = "floating enable";
            criteria = {
              class = "^net-filebot-Main$";
            };
          }
          {
            command = "floating enable";
            criteria = {
              class = "^ROX-Filer$";
            };
          }
        ];
      };

      floating = {
        modifier = "Mod4";
      };

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
        "${cfg.config.modifier}+n" = "focus left";
        "${cfg.config.modifier}+e" = "focus down";
        "${cfg.config.modifier}+i" = "focus up";
        "${cfg.config.modifier}+o" = "focus right";

        # move focused window
        "${cfg.config.modifier}+Shift+n" = "move left";
        "${cfg.config.modifier}+Shift+e" = "move down";
        "${cfg.config.modifier}+Shift+i" = "move up";
        "${cfg.config.modifier}+Shift+o" = "move right";

        # split in horizontal orientation
        "${cfg.config.modifier}+h" = "split h";

        # split in vertical orientation
        "${cfg.config.modifier}+v" = "split v";

        # change focus between output
        "${cfg.config.modifier}+$alt+o" = "focus output right";
        "${cfg.config.modifier}+$alt+n" = "focus output left";

        # move workspaces between monitors
        "${cfg.config.modifier}+Shift+$alt+o" = "move workspace to output right";
        "${cfg.config.modifier}+Shift+$alt+n" = "move workspace to output left";

        # toggle sticky
        "${cfg.config.modifier}+s" = "sticky toggle";

        # toggle tiling / floating
        "$alt+Shift+space" = "floating toggle";

        # change focus between tiling / floating windows
        "$alt+space" = "focus mode_toggle";

        # enter fullscreen mode for the focused container
        "${cfg.config.modifier}+f" = "fullscreen toggle";

        # kill focused window
        "${cfg.config.modifier}+Shift+q" = "kill";

        # rbrowser
        "${cfg.config.modifier}+b" = "exec ${pkgs.rbrowser}/bin/rbrowser";

        # rofi run
        "${cfg.config.modifier}+r" = "exec ${pkgs.rofi}/bin/rofi -show run";

        # rofi lastpass
        "${cfg.config.modifier}+p" = "exec ${pkgs.rofi}/bin/rofi -modi lpass:rofi-lpass -show lpass";

        # list open windows to switch to
        "$alt+Tab" = "exec ${pkgs.rofi}/bin/rofi -show window";

        # switch between the current and the previously focused one
        "${cfg.config.modifier}+Tab" = "workspace back_and_forth";
        "${cfg.config.modifier}+Shift+Tab" = "move container to workspace back_and_forth";

        # dynamic workspaces
        "${cfg.config.modifier}+space" = "exec ${pkgs.rofi}/bin/rofi -show i3Workspaces";
        "${cfg.config.modifier}+Shift+space" = "exec ${pkgs.rofi}/bin/rofi -show i3MoveContainer";
        "${cfg.config.modifier}+$alt+space" = "exec ${pkgs.rofi}/bin/rofi -show i3RenameWorkspace";

        # change container layout (stacked, tabbed, toggle split)
        "${cfg.config.modifier}+l" = "layout stacking";
        "${cfg.config.modifier}+u" = "layout tabbed";
        "${cfg.config.modifier}+y" = "layout toggle split";

        # focus the parent container
        "${cfg.config.modifier}+a" = "focus parent";

        # focus the child container
        "${cfg.config.modifier}+d" = "focus child";

        # start a region screenshot
        "${cfg.config.modifier}+Shift+4" = "exec ${pkgs.maim}/bin/maim -s ~/Desktop/screenshot-`date +%Y-%m-%d_%H:%M:%S`.png";

        # focus the urgent window
        "${cfg.config.modifier}+x" = "[urgent=latest] focus";

        # mark current window / goto mark
        # https://github.com/tybitsfox/i3msg/blob/master/.i3/config
        "${cfg.config.modifier}+m" = "exec i3-input -F 'mark %s' -l 1 -P 'Mark: '";
        "${cfg.config.modifier}+apostrophe" = "exec i3-input -F '[con_mark=\"%s\"] focus' -l 1 -P 'Go to: '";

        # volume support
        "XF86AudioRaiseVolume" = "exec $nosid ${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ false, exec $nosid ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ +5%";
        "XF86AudioLowerVolume" = "exec $nosid ${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ false, exec $nosid ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ -5%";
        "XF86AudioMute" = "exec $nosid ${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle";

        # brightness support
        "XF86MonBrightnessUp" = "exec $nosid ${pkgs.brightnessctl}/bin/brightnessctl s +5%";
        "XF86MonBrightnessDown" = "exec $nosid ${pkgs.brightnessctl}/bin/brightnessctl s 5%-";
        "Shift+XF86MonBrightnessUp" = "exec $nosid ${pkgs.brightnessctl}/bin/brightnessctl s +1%";
        "Shift+XF86MonBrightnessDown" = "exec $nosid ${pkgs.brightnessctl}/bin/brightnessctl s 1%-";

        # sleep support
        "XF86PowerOff" = "exec $nosid $locker && systemctl suspend";

        # clipboard history
        "${cfg.config.modifier}+$alt+c" = "exec ${pkgs.rofi}/bin/rofi -modi \"clipboard:${pkgs.haskellPackages.greenclip}/bin/greenclip print\" -show clipboard";
      };
    };
  };
}
