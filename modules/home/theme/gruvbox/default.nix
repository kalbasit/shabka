{ config, pkgs, lib, ... }:

with pkgs;
with lib;

let

  grovboxVersion = "3.0.1-rc.0";

  grovboxSrc = fetchFromGitHub {
    owner = "morhetz";
    repo = "gruvbox";
    rev = "v${grovboxVersion}";
    sha256 = "01as1pkrlbzhcn1kyscy476w8im3g3wmphpcm4lrx7nwdq8ch7h1";
  };

  neovimExtraKnownPlugins = {
    vim-color-gruvbox = vimUtils.buildVimPluginFrom2Nix {
      name = "gruvbox-${grovboxVersion}";
      src = grovboxSrc;
      dependencies = [];
    };
  };

in {
  # TODO(medium): offer the light version of the theme
  config = mkMerge [
    (mkIf (config.shabka.theme == "gruvbox-dark") {
      # Setup the environment correctly with the shell palette
      programs.zsh.initExtra = ''
        source ${grovboxSrc}/gruvbox_256palette.sh
      '';

      shabka.workstation.alacritty.extraRC = ''
        # Colors (Gruvbox dark)
        colors:
          # Default colors
          primary:
            # hard contrast: background = '0x1d2021'
            background: '0x282828'
            # soft contrast: background = '0x32302f'
            foreground: '0xebdbb2'

          # Normal colors
          normal:
            black:   '0x282828'
            red:     '0xcc241d'
            green:   '0x98971a'
            yellow:  '0xd79921'
            blue:    '0x458588'
            magenta: '0xb16286'
            cyan:    '0x689d6a'
            white:   '0xa89984'

          # Bright colors
          bright:
            black:   '0x928374'
            red:     '0xfb4934'
            green:   '0xb8bb26'
            yellow:  '0xfabd2f'
            blue:    '0x83a598'
            magenta: '0xd3869b'
            cyan:    '0x8ec07c'
            white:   '0xebdbb2'
      '';

      shabka.neovim = {
        extraRC = ''
          set background=dark
          colorscheme gruvbox
          let g:airline_theme='gruvbox'
        '';

        extraKnownPlugins = neovimExtraKnownPlugins;

        extraPluginDictionaries = [{
          names = [
            "vim-color-gruvbox"
          ];
        }];
      };

      # Originally found this at
      # https://github.com/a-schaefers/i3-wm-gruvbox-theme/blob/f6e570d6ab11b00b950e993c8619ac253bbb03ea/i3/config#L101
      #
      # TODO(low): import the green variation from
      # https://github.com/a-schaefers/i3-wm-gruvbox-theme/blob/f6e570d6ab11b00b950e993c8619ac253bbb03ea/i3/config#L136-L141
      xsession = optionalAttrs stdenv.isLinux {
        windowManager.i3.config =
          let
          # hard contrast: bg = '#282828'
          bg       = "#282828";
          # soft contrast: bg = '#32302f'

          aqua     = "#689d68";
          blue     = "#458588";
          darkgray = "#1d2021";
          gray     = "#a89984";
          green    = "#98971a";
          purple   = "#b16286";
          red      = "#cc241d";
          white    = "#ebdbb2";
          yellow   = "#d79921";

          in {
            bars = optionals config.shabka.workstation.i3.bar.i3bar.enable
              [{
                position = config.shabka.workstation.i3.bar.location;

                colors = {
                  background = bg;
                  statusline = yellow;
                  separator = red;

                  focusedWorkspace = {
                    border = aqua; background = aqua; text = darkgray;
                  };

                  activeWorkspace = {
                    border = darkgray; background = darkgray; text = yellow;
                  };

                  inactiveWorkspace = {
                    border = darkgray; background = darkgray; text = yellow;
                  };

                  urgentWorkspace = {
                    border = red; background = red; text = bg;
                  };
                };
              }];

            colors = {
              background = darkgray;

              focused = {
                border = blue; background = blue; text = darkgray; indicator = purple; childBorder = darkgray;
              };

              focusedInactive = {
                border = darkgray; background = darkgray; text = yellow; indicator = purple; childBorder = darkgray;
              };

              unfocused = {
                border = darkgray; background = darkgray; text = yellow; indicator = purple; childBorder = darkgray;
              };

              urgent = {
                border = red; background = red; text = white; indicator = red; childBorder = red;
              };
            };
          };
        };

      services.polybar.config."colors" = {
        background = "#282828";
        background-alt = "#689d68";
        foreground = "#ebdbb2";
        foreground-alt = "#ebdbb2";
        primary = "#689d68";
        secondary = "#1d2021";
        alert = "#cc241d";
      };

      programs.rofi.theme = "gruvbox-dark";

      programs.termite = {
        # hard contrast: backgroundColor = "#1d2021";
        backgroundColor = "#282828";
        # soft contrast: backgroundColor = "#32302f";

        foregroundColor = "#ebdbb2";
        foregroundBoldColor = "#ebdbb2";
        colorsExtra = ''
          # dark0 + gray
          color0 = #282828
          color8 = #928374

          # neutral_red + bright_red
          color1 = #cc241d
          color9 = #fb4934

          # neutral_green + bright_green
          color2 = #98971a
          color10 = #b8bb26

          # neutral_yellow + bright_yellow
          color3 = #d79921
          color11 = #fabd2f

          # neutral_blue + bright_blue
          color4 = #458588
          color12 = #83a598

          # neutral_purple + bright_purple
          color5 = #b16286
          color13 = #d3869b

          # neutral_aqua + faded_aqua
          color6 = #689d6a
          color14 = #8ec07c

          # light4 + light1
          color7 = #a89984
          color15 = #ebdbb2
        '';
      };

      # Taken from https://github.com/x4121/dotfiles/blob/4e73c297afe7675bc5490fbb73b8f2481cf3ca95/etc/gruvbox-dark-256.taskwarrior.theme
      programs.taskwarrior.extraConfig = ''
        ################################################################################
        #
        # Copyright 2017, Armin Grodon.
        # Copyright 2006 - 2016, Paul Beckingham, Federico Hernandez.
        #
        # Permission is hereby granted, free of charge, to any person obtaining a copy
        # of this software and associated documentation files (the "Software"), to deal
        # in the Software without restriction, including without limitation the rights
        # to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
        # copies of the Software, and to permit persons to whom the Software is
        # furnished to do so, subject to the following conditions:
        #
        # The above copyright notice and this permission notice shall be included
        # in all copies or substantial portions of the Software.
        #
        # THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
        # OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
        # FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
        # THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
        # LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
        # OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
        # SOFTWARE.
        #
        # http://www.opensource.org/licenses/mit-license.php
        #
        ###############################################################################

        rule.precedence.color=deleted,completed,active,keyword.,tag.,project.,overdue,scheduled,due.today,due,blocked,blocking,recurring,tagged,uda.

        # General decoration
        color.label=
        color.label.sort=
        color.alternate=on color237
        color.header=color4
        color.footnote=color6
        color.warning=color0 on color3
        color.error=color1
        color.debug=color5

        # Task state
        color.completed=
        color.deleted=
        color.active=color11
        color.recurring=color4
        color.scheduled=
        color.until=
        color.blocked=color0 on color3
        color.blocking=color9 on color3

        # Project
        color.project.none=

        # Priority
        color.uda.priority.H=color13
        color.uda.priority.M=color12
        color.uda.priority.L=color14

        # Tags
        color.tag.next=
        color.tag.none=
        color.tagged=color10

        # Due
        color.due=color9
        color.due.today=color1
        color.overdue=color5

        # Report: burndown
        color.burndown.done=color0 on color2
        color.burndown.pending=color0 on color1
        color.burndown.started=color0 on color3

        # Report: history
        color.history.add=color0 on color1
        color.history.delete=color0 on color3
        color.history.done=color0 on color2

        # Report: summary
        color.summary.background=on color0
        color.summary.bar=color0 on color2

        # Command: calendar
        color.calendar.due=color0 on color3
        color.calendar.due.today=color0 on color166
        color.calendar.overdue=color0 on color1
        color.calendar.holiday=color0 on color6
        color.calendar.today=color0 on color4
        color.calendar.weekend=color14 on color0
        color.calendar.weeknumber=color12

        # Command: sync
        color.sync.added=color10
        color.sync.changed=color11
        color.sync.rejected=color9

        # Command: undo
        color.undo.after=color2
        color.undo.before=color1
      '';

      programs.tmux.extraConfig = ''
        # pane number display
        set-option -g display-panes-active-colour colour250 #fg2
        set-option -g display-panes-colour colour237 #bg1

        # clock
        set-window-option -g clock-mode-colour colour109 #blue

        # bell
        set-window-option -g window-status-bell-style fg=colour235,bg=colour167 #bg, red

        ## Theme settings mixed with colors (unfortunately, but there is no cleaner way)
        set-option -g status-justify "left"
        set-option -g status-left-length "80"
        set-option -g status-right-length "80"
        set-window-option -g window-status-separator ""

        set-option -g status-left "#[fg=colour248, bg=colour241] #S #[fg=colour241, bg=colour237, nobold, noitalics, nounderscore]"
        set-option -g status-right "#[fg=colour239, bg=colour237, nobold, nounderscore, noitalics]#{prefix_highlight}#[fg=colour246,bg=colour239] %Y-%m-%d  %H:%M #[fg=colour248, bg=colour239, nobold, noitalics, nounderscore]#[fg=colour237, bg=colour248] #h "

        set-window-option -g window-status-current-format "#[fg=colour239, bg=colour248, :nobold, noitalics, nounderscore]#[fg=colour239, bg=colour214] #I #[fg=colour239, bg=colour214, bold] #W #[fg=colour214, bg=colour237, nobold, noitalics, nounderscore]"
        set-window-option -g window-status-format "#[fg=colour237,bg=colour239,noitalics]#[fg=colour223,bg=colour239] #I #[fg=colour223, bg=colour239] #W #[fg=colour239, bg=colour237, noitalics]"
      ''
      + (if versionAtLeast (builtins.parseDrvName pkgs.tmux.name).version "2.9" then ''
        # default statusbar colors
        set-option -g status-style bg=colour237,fg=colour223,none
        set-option -g status-left-style none
        set-option -g status-right-style none

        # default window title colors
        set-window-option -g window-status-style bg=colour214,fg=colour237,none

        set-window-option -g window-status-activity-style bg=colour237,fg=colour248,none

        # active window title colors
        set-window-option -g window-status-current-style bg=default,fg=colour237

        # pane border
        set-option -g pane-active-border-style bg=colour250,fg=colour237
        set-option -g pane-border-style bg=colour237,fg=colour250

        # message infos
        set-option -g message-style bg=colour239,fg=colour223

        # writting commands inactive
        set-option -g message-command-style bg=colour239,fg=colour223
      '' else ''
        # default statusbar colors
        set-option -g status-bg colour237 #bg1
        set-option -g status-fg colour223 #fg1

        # default window title colors
        set-window-option -g window-status-bg colour214 #yellow
        set-window-option -g window-status-fg colour237 #bg1

        set-window-option -g window-status-activity-bg colour237 #bg1
        set-window-option -g window-status-activity-fg colour248 #fg3

        # active window title colors
        set-window-option -g window-status-current-bg default
        set-window-option -g window-status-current-fg colour237 #bg1

        # pane border
        set-option -g pane-active-border-fg colour250 #fg2
        set-option -g pane-border-fg colour237 #bg1

        # message infos
        set-option -g message-bg colour239 #bg2
        set-option -g message-fg colour223 #fg1

        # writting commands inactive
        set-option -g message-command-bg colour239 #fg3
        set-option -g message-command-fg colour223 #bg1

        set-option -g status-attr "none"
        set-option -g status-left-attr "none"
        set-option -g status-right-attr "none"
        set-window-option -g window-status-activity-attr "none"
        set-window-option -g window-status-attr "none"
      '');
    })

    (mkIf (config.shabka.theme == "gruvbox-dark" && config.shabka.darwinConfig != {}) {
      home.file = {
        "Library/Application Support/iTerm2/DynamicProfiles/gruvbox-dark-sourcecode-pro.json".source = ./iterm-dynamic-profile.json;
      };
    })
  ];
}
