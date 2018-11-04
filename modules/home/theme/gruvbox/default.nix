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
  config = mkIf (config.mine.theme == "gruvbox-dark") {
    # Setup the environment correctly with the shell palette
    programs.zsh.initExtra = ''
      source ${grovboxSrc}/gruvbox_256palette.sh
    '';

    mine.workstation.alacritty.extraRC = ''
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

    mine.neovim = {
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
    xsession.windowManager.i3.config =
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
        bars = [{
          position = "top";

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
  };
}
