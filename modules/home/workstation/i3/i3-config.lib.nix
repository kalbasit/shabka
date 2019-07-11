{ config, pkgs, lib, ... }:

with lib;

let
  defaultModifier = "Mod4"; # <Super>
  secondModifier = "Shift";
  thirdModifier = "Mod1"; # <Alt>

  nosid = "--no-startup-id";

  locker = "${getBin pkgs.xautolock}/bin/xautolock -locknow && sleep 1";
  urxvt = pkgs.rxvt_unicode;

  mode_resize = "resize";
  mode_system = "(l)ock, (e)xit, (s)uspend, h(y)brid sleep, (h)ibernate, (r)eboot, (Shift+s)hutdown"; # TODO: update those shortcuts

  ws1 = "1:🔥";
  ws2 = "2";
  ws3 = "3";
  ws4 = "4:🦊";
  ws5 = "5";
  ws6 = "6";
  ws7 = "7:✉️";
  ws8 = "8:🌐";
  wst = "10:💻";

  autoStart = pkgs.writeScript "i3-autostart.sh" ''
    #! /usr/bin/env bash
    set -euo pipefail
    # Wait for the rest to complete
    ${pkgs.coreutils}/bin/sleep 5
    # Workspace 1
    ${pkgs.i3}/bin/i3-msg "workspace ${ws1}"
    # TODO: try to restore layout using append_layout
    ${urxvt}/bin/urxvt -T cli_is_love &
    ${urxvt}/bin/urxvt -T cli_is_life &
    ${urxvt}/bin/urxvt -T monitor -e ${pkgs.python37Packages.glances}/bin/glances &
    ${pkgs.coreutils}/bin/sleep 1
    # Workspace T
    ${pkgs.i3}/bin/i3-msg "workspace ${wst}"
    ${pkgs.i3}/bin/i3-msg "layout tabbed"
    ${urxvt}/bin/urxvt -T duck -e mosh risson@duck.risson.space -- /bin/sh -c 'tmux has-session && exec tmux attach || exec tmux' &
    ${pkgs.coreutils}/bin/sleep 2
    # Workspace 8
    ${pkgs.i3}/bin/i3-msg "workspace ${ws8}"
    ${pkgs.i3}/bin/i3-msg "layout tabbed"
    ${urxvt}/bin/urxvt -T weechat -e mosh risson@irc.risson.space -- /bin/sh -c 'screen -x weechat-risson' &
    ${pkgs.yubioath-desktop}/bin/yubiaoth-desktop &
    ${pkgs.blueman}/bin/blueman-manager &
  '';

in {
  enable = true;
  package = pkgs.i3;

  config = {
    modifier = defaultModifier;

    fonts = [ "pango:SourceCodePro Regular 8" ];

    bars = []; # We are using Polybar, so no bar should be defined

    window = {
      titlebar = false; # Configure border style <pixel xx>
      border = 0; # Configure border style <xx 0>
      hideEdgeBorders = "smart"; # Hide borders
    };

    floating = {
      titlebar = true; # Configure border style <normal xx>
      border = 2; # Configure border style <xx 2>
      criteria = [
        { class = "Pavucontrol"; }
        { class = "qt5ct"; }
        { class = "Qtconfig-qt4"; }
        { class = "Yad"; title = "yad-calendar"; }
        { title = "alsamixer"; }
        { title = "File Transfer*"; }
        { title = "i3_help"; }
        { title = "Musescore: Play Panel"; }
      ];
      modifier = defaultModifier;
    };

    focus = {
      followMouse = true;
      mouseWarping = true;
      newWindow = "urgent";
    };

    workspaceLayout = "default";

    startup = [
      { command = "${getBin pkgs.thunderbird}/bin/thunderbird"; always = false; notification = false; }
      { command = "${getBin pkgs.firefox}/bin/firefox"; always = false; notification = false; }
      { command = "${autoStart}"; always = false; notification = false; }
    ];

    modes = {
      "${mode_resize}" = {
        "c" = "resize shrink width 10 px or 10 ppt";
        "t" = "resize grow height 10 px or 10 ppt";
        "s" = "resize shrink height 10 px or 10 ppt";
        "r" = "resize grow width 10 px or 10 ppt";
        "Left" = "resize shrink width 10 px or 10 ppt";
        "Down" = "resize grow height 10 px or 10 ppt";
        "Up" = "resize shrink height 10 px or 10 ppt";
        "Right" = "resize grow width 10 px or 10 ppt";

        "Return" = "mode default";
        "Escape" = "mode default";
      };

      "${mode_system}" = {
        "r" = "exec ${nosid} ${locker}, mode default";
        "u" = "exec ${nosid} ${locker} && systemctl suspend, mode default";
        "^" = "exec ${nosid} ${locker} && systemctl hybrid-sleep, mode default";
        "p" = "exit";
        "c" = "exec ${nosid} ${locker} && systemctl hibernate, mode default";
        "o" = "exec ${nosid} systemctl reboot";
        "Shift+U" = "exec ${nosid} systemctl poweroff -i";

        "Return" = "mode default";
        "Escape" = "mode default";
      };
    };

    keybindings = {
      # change borders
      "${defaultModifier}+d" = "border none";
      "${defaultModifier}+g" = "border normal";
      # start a terminal
      "${defaultModifier}+Return" = "exec i3-sensible-terminal";
      # kill focused window
      "${defaultModifier}+${secondModifier}+B" = "kill";
      # start dmenu (a program launcher), which is actually rofi
      "${defaultModifier}+i" = "exec ${getBin pkgs.rofi}/bin/rofi -show run";
      # list open windows to switch to
      "${thirdModifier}+Tab" = "exec ${getBin pkgs.rofi}/bin/rofi -show window";
      # change focus
      "${defaultModifier}+c" = "focus left";
      "${defaultModifier}+t" = "focus down";
      "${defaultModifier}+s" = "focus up";
      "${defaultModifier}+r" = "focus right";
      "${defaultModifier}+Left" = "focus left";
      "${defaultModifier}+Down" = "focus down";
      "${defaultModifier}+Up" = "focus up";
      "${defaultModifier}+Right" = "focus right";
      # move focused window
      "${defaultModifier}+${secondModifier}+C" = "move left";
      "${defaultModifier}+${secondModifier}+T" = "move down";
      "${defaultModifier}+${secondModifier}+S" = "move up";
      "${defaultModifier}+${secondModifier}+R" = "move right";
      "${defaultModifier}+${secondModifier}+Left" = "move left";
      "${defaultModifier}+${secondModifier}+Down" = "move down";
      "${defaultModifier}+${secondModifier}+Up" = "move up";
      "${defaultModifier}+${secondModifier}+Right" = "move right";
      # workspace back and forth (with/without active container)
      "${defaultModifier}+k" = "workspace back_and_forth";
      "${defaultModifier}+${secondModifier}+k" = "move container to workspace back_and_forth; workspace back_and_forth";
      # split orientation
      "${defaultModifier}+egrave" = "split h;exec notify-send 'tile horizontally'";
      "${defaultModifier}+period" = "split v;exec notify-send 'tile vertically'";
      # enter fullscreen mode for the focused container
      "${defaultModifier}+e" = "fullscreen toggle";
      # change container layout (stacked, tabbed, toggle split)
      "${defaultModifier}+u" = "layout stacking";
      "${defaultModifier}+eacute" = "layout tabbed";
      "${defaultModifier}+p" = "layout toggle split";
      # toggle tiling / floating
      "${defaultModifier}+${secondModifier}+space" = "floating toggle";
      # change focus between tiling / floating windows
      "${defaultModifier}+space" = "focus mode_toggle";
      # focus the parent container
      "${defaultModifier}+a" = "focus parent";
      # switch to workspace
      "${defaultModifier}+quotedbl" = "workspace ${ws1}";
      "${defaultModifier}+guillemotleft" = "workspace ${ws2}";
      "${defaultModifier}+guillemotright" = "workspace ${ws3}";
      "${defaultModifier}+parenleft" = "workspace ${ws4}";
      "${defaultModifier}+parenright" = "workspace ${ws5}";
      "${defaultModifier}+at" = "workspace ${ws6}";
      "${defaultModifier}+plus" = "workspace ${ws7}";
      "${defaultModifier}+minus" = "workspace ${ws8}";
      "${defaultModifier}+equal" = "workspace ${wst}";
      # move focused container to workspace
      "${defaultModifier}+Ctrl+quotedbl" = "move container to workspace ${ws1}";
      "${defaultModifier}+Ctrl+guillemotleft" = "move container to workspace ${ws2}";
      "${defaultModifier}+Ctrl+guillemotright" = "move container to workspace ${ws3}";
      "${defaultModifier}+Ctrl+parenleft" = "move container to workspace ${ws4}";
      "${defaultModifier}+Ctrl+parenright" = "move container to workspace ${ws5}";
      "${defaultModifier}+Ctrl+at" = "move container to workspace ${ws6}";
      "${defaultModifier}+Ctrl+plus" = "move container to workspace ${ws7}";
      "${defaultModifier}+Ctrl+minus" = "move container to workspace ${ws8}";
      "${defaultModifier}+Ctrl+equal" = "move container to workspace ${wst}";
      # move to workspace with focused container
      "${defaultModifier}+${secondModifier}+quotedbl" = "move container to workspace ${ws1}; workspace ${ws1}";
      "${defaultModifier}+${secondModifier}+guillemotleft" = "move container to workspace ${ws2}; workspace ${ws2}";
      "${defaultModifier}+${secondModifier}+guillemotright" = "move container to workspace ${ws3}; workspace ${ws3}";
      "${defaultModifier}+${secondModifier}+parenleft" = "move container to workspace ${ws4}; workspace ${ws4}";
      "${defaultModifier}+${secondModifier}+parenright" = "move container to workspace ${ws5}; workspace ${ws5}";
      "${defaultModifier}+${secondModifier}+at" = "move container to workspace ${ws6}; workspace ${ws6}";
      "${defaultModifier}+${secondModifier}+plus" = "move container to workspace ${ws7}; workspace ${ws7}";
      "${defaultModifier}+${secondModifier}+minus" = "move container to workspace ${ws8}; workspace ${ws8}";
      "${defaultModifier}+${secondModifier}+equal" = "move container to workspace ${wst}; workspace ${wst}";
      # reload the configuration file
      "${defaultModifier}+${secondModifier}+X" = "reload";
      # restart i3 inplace (preserves your layout/session, can be used to upgrade i3)
      "${defaultModifier}+${secondModifier}+O" = "restart";
      # switch to mode resize, to resize window
      "${defaultModifier}+o" = "mode ${mode_resize}";
      # exit i3 (logs you out of your X session)
      "${defaultModifier}+Shift+P" = "exec \"i3-nagbar -t warning -m 'You pressed the exit shortcut. Do you really want to exit i3? This will end your X session.' -B 'Yes, exit i3' 'i3-msg exit'\"";
      # lock screen
      "${defaultModifier}+slash" = "exec ${nosid} ${locker}";
      # switch to mode_system
      "${defaultModifier}+asterisk" = "mode ${mode_system}";
      # hide/unhide polybar
      "${defaultModifier}+q" = "exec ${nosid} polybar-msg cmd toggle";
      # custom shortcuts for applications
      "${defaultModifier}+F1" = "exec firefox";
      "${defaultModifier}+F4" = "exec pcmanfm";
      # pulseaudio controls
      "XF86AudioRaiseVolume" = "exec ${nosid} ${getBin pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ false, exec ${nosid} ${getBin pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ +5%";
      "XF86AudioLowerVolume" = "exec ${nosid} ${getBin pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ false, exec ${nosid} ${getBin pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ -5%";
      "XF86AudioMute" = "exec ${nosid} ${getBin pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle";
      # screenshots
      "Print" = "exec ${nosid} ${getBin pkgs.flameshot}/bin/flameshot full -c -p \"/home/risson/Pictures/Screenshots\"";
      "${defaultModifier}+Print" = "exec ${nosid} flameshot gui";
      # brightness
      "XF86MonBrightnessUp" = "exec ${nosid} ${getBin pkgs.brightnessctl}/bin/brightnessctl s +5%";
      "XF86MonBrightnessDown" = "exec ${nosid} ${getBin pkgs.brightnessctl}/bin/brightnessctl s -5%";
      "${secondModifier}+XF86MonBrightnessUp" = "exec ${nosid} ${getBin pkgs.brightnessctl}/bin/brightnessctl s +1%";
      "${secondModifier}+XF86MonBrightnessDown" = "exec ${nosid} ${getBin pkgs.brightnessctl}/bin/brightnessctl s -1%";
      # sleep
      "XF86PowerOff" = "exec ${nosid} ${locker} && systemctl suspend";
      # clipboard history
      "${defaultModifier}+${thirdModifier}+x" = "exec ${getBin pkgs.rofi}/bin/rofi -modi \"clipboard:${getBin pkgs.haskellPackages.greenclip}/bin/greenclip print\" -show clipboard";
    };
  };
  extraConfig = ''
    workspace_auto_back_and_forth yes
  '';
}