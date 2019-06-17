{ config, pkgs, lib, ... }:

with lib;

let

  neovimExtraKnownPlugins = pkgs.callPackage ./plugins.lib.nix {};

in {
  # TODO(medium): offer the light version of this theme.
  config = mkIf (config.mine.theme == "seoul256") {
    mine.workstation.alacritty.extraRC = ''
      # When true, bold text is drawn using the bright variant of colors.
      draw_bold_text_with_bright_colors: true

      # Use custom cursor colors. If true, display the cursor in the cursor.foreground
      # and cursor.background colors, otherwise invert the colors of the cursor.
      custom_cursor_colors: false

      # Colors (Seoul256)
      colors:
        # Default colors
        primary:
          background: '0x3a3a3a'
          foreground: '0xd0d0d0'

        # Normal colors
        normal:
          black:   '0x4e4e4e'
          red:     '0xd68787'
          green:   '0x5f865f'
          yellow:  '0xd8af5f'
          blue:    '0x85add4'
          magenta: '0xd7afaf'
          cyan:    '0x87afaf'
          white:   '0xd0d0d0'

        # Bright colors
        bright:
          black:   '0x626262'
          red:     '0xd75f87'
          green:   '0x87af87'
          yellow:  '0xffd787'
          blue:    '0xadd4fb'
          magenta: '0xffafaf'
          cyan:    '0x87d7d7'
          white:   '0xe4e4e4'
    '';

    mine.neovim = {
      extraRC = ''
        colorscheme seoul256
        let g:airline_theme='seoul256'
      '';

      extraKnownPlugins = neovimExtraKnownPlugins;

      extraPluginDictionaries = [{
        names = [
          "airline-seoul256-theme"
          "vim-color-seoul256"
        ];
      }];
    };

    xsession = optionalAttrs pkgs.stdenv.isLinux {
      windowManager.i3.config = {
        bars = optional (config.mine.workstation.i3.bar == "i3bar")
          [{
            position = "top";

            colors = {
              background = "#626262";
              statusline = "#00ff00";
              separator = "#d68787";
              focusedWorkspace = { border = "#e4e4e4"; background = "#5f865f"; text = "#e4e4e4"; };
              activeWorkspace = { border = "#e4e4e4"; background = "#ffafaf"; text = "#e4e4e4"; };
              inactiveWorkspace = { border = "#87af87"; background = "#87af87"; text = "#626262"; };
              urgentWorkspace = { border = "#ff0000"; background = "#ff0000"; text = "#e4e4e4"; };
            };
          }];

        colors = {
          background = "#4e4e4e";

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
      };
    };

    services.polybar.config."colors" = {
      background = "#5f865f";
      background-alt = "#87af87";
      foreground = "#e4e4e4";
      foreground-alt = "#626262";
      primary = "#ffafaf";
      secondary = "#5f676a";
      alert = "#ff0000";
    };

    programs.rofi.theme = "Adapta-Nokto";

    programs.termite = {
      backgroundColor = "#3a3a3a";
      foregroundColor = "#d0d0d0";
      colorsExtra = ''
        color0     = #4e4e4e
        color10    = #87af87
        color11    = #ffd787
        color12    = #add4fb
        color13    = #ffafaf
        color14    = #87d7d7
        color15    = #e4e4e4
        color1     = #d68787
        color2     = #5f865f
        color3     = #d8af5f
        color4     = #85add4
        color5     = #d7afaf
        color6     = #87afaf
        color7     = #d0d0d0
        color8     = #626262
        color9     = #d75f87
      '';
    };

    programs.taskwarrior.colorTheme = "solarized-dark-256";
  };
}
