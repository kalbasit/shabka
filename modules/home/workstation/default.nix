{ config, pkgs, lib, ... }:

with lib;

with import ../../../util;

{
  options.mine.workstation.enable = mkEnableOption "Workstation Profile";

  config = mkIf config.mine.workstation.enable {
    services.flameshot.enable = true;
    services.network-manager-applet.enable = true;

    home = {
      packages = with pkgs; [
        nur.repos.kalbasit.rbrowser

        remmina

        weechat

        xsel

        # zoom for meetings
        zoom-us
      ];

      activation = {
        rbrowser-desktop-link = symlink
        "${pkgs.nur.repos.kalbasit.rbrowser}/share/applications/rbrowser.desktop"
        "${config.home.homeDirectory}/.local/share/applications/rbrowser.desktop";
      };

      file = {
        ".local/share/applications/mimeapps.list".text = let
          mimeTypes =
            [
              "application/pdf"
              "application/xhtml+xml"
              "text/html"
              "text/xml"
              "x-scheme-handler/about"
              "x-scheme-handler/http"
              "x-scheme-handler/https"
              "x-scheme-handler/unknown"
            ];

          rbrowser = builtins.concatStringsSep
            "\n"
            (map (typ: "${typ}=rbrowser.desktop") mimeTypes);

        in ''
          [Default Applications]
          ${rbrowser}
        '';
      };
    };

    programs.zsh.initExtra = ''
      # Set the browser to my relay browser
      export BROWSER="${pkgs.nur.repos.kalbasit.rbrowser}/bin/rbrowser"
    '';

    mine.workstation = enableMultiple [
      "alacritty"
      "autorandr"
      "chromium"
      "dropbox"
      "dunst"
      "firefox"
      "greenclip"
      "locker"
      "mysql-workbench"
      "rofi"
      "termite"
    ];
  };
}
