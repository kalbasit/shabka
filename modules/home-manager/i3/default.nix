{ pkgs, ... }:

let
  defaultModifier = "Mod4";
in {
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
        modifier = "${defaultModifier}";
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
        "${defaultModifier}+n" = "focus left";
        "${defaultModifier}+e" = "focus down";
        "${defaultModifier}+i" = "focus up";
        "${defaultModifier}+o" = "focus right";

        # move focused window
        "${defaultModifier}+Shift+n" = "move left";
        "${defaultModifier}+Shift+e" = "move down";
        "${defaultModifier}+Shift+i" = "move up";
        "${defaultModifier}+Shift+o" = "move right";

        # split in horizontal orientation
        "${defaultModifier}+h" = "split h";

        # split in vertical orientation
        "${defaultModifier}+v" = "split v";

        # change focus between output
        "${defaultModifier}+$alt+o" = "focus output right";
        "${defaultModifier}+$alt+n" = "focus output left";

        # move workspaces between monitors
        "${defaultModifier}+Shift+$alt+o" = "move workspace to output right";
        "${defaultModifier}+Shift+$alt+n" = "move workspace to output left";

        # toggle sticky
        "${defaultModifier}+s" = "sticky toggle";

        # toggle tiling / floating
        "$alt+Shift+space" = "floating toggle";

        # change focus between tiling / floating windows
        "$alt+space" = "focus mode_toggle";

        # enter fullscreen mode for the focused container
        "${defaultModifier}+f" = "fullscreen toggle";

        # kill focused window
        "${defaultModifier}+Shift+q" = "kill";

        # rbrowser
        "${defaultModifier}+b" = "exec ${pkgs.rbrowser}/bin/rbrowser";

        # rofi run
        "${defaultModifier}+r" = "exec ${pkgs.rofi}/bin/rofi -show run";

        # rofi lastpass
        "${defaultModifier}+p" = "exec ${pkgs.rofi}/bin/rofi -modi lpass:rofi-lpass -show lpass";

        # list open windows to switch to
        "$alt+Tab" = "exec ${pkgs.rofi}/bin/rofi -show window";

        # switch between the current and the previously focused one
        "${defaultModifier}+Tab" = "workspace back_and_forth";
        "${defaultModifier}+Shift+Tab" = "move container to workspace back_and_forth";

        # dynamic workspaces
        "${defaultModifier}+space" = "exec ${pkgs.rofi}/bin/rofi -show i3Workspaces";
        "${defaultModifier}+Shift+space" = "exec ${pkgs.rofi}/bin/rofi -show i3MoveContainer";
        "${defaultModifier}+$alt+space" = "exec ${pkgs.rofi}/bin/rofi -show i3RenameWorkspace";

        # change container layout (stacked, tabbed, toggle split)
        "${defaultModifier}+l" = "layout stacking";
        "${defaultModifier}+u" = "layout tabbed";
        "${defaultModifier}+y" = "layout toggle split";

        # focus the parent container
        "${defaultModifier}+a" = "focus parent";

        # focus the child container
        "${defaultModifier}+d" = "focus child";

        # start a region screenshot
        "${defaultModifier}+Shift+4" = "exec ${pkgs.maim}/bin/maim -s ~/Desktop/screenshot-`date +%Y-%m-%d_%H:%M:%S`.png";

        # focus the urgent window
        "${defaultModifier}+x" = "[urgent=latest] focus";

        # mark current window / goto mark
        # https://github.com/tybitsfox/i3msg/blob/master/.i3/config
        "${defaultModifier}+m" = "exec i3-input -F 'mark %s' -l 1 -P 'Mark: '";
        "${defaultModifier}+apostrophe" = "exec i3-input -F '[con_mark=\"%s\"] focus' -l 1 -P 'Go to: '";

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
        "${defaultModifier}+$alt+c" = "exec ${pkgs.rofi}/bin/rofi -modi \"clipboard:${pkgs.haskellPackages.greenclip}/bin/greenclip print\" -show clipboard";
      };
    };
  };
}
