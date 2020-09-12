{ config, pkgs, lib, ... }:

with lib;

let
  mimeList =
    let
      mimeTypes =
        [
          "application/pdf"
          "application/x-extension-htm"
          "application/x-extension-html"
          "application/x-extension-shtml"
          "application/x-extension-xht"
          "application/x-extension-xhtml"
          "application/xhtml+xml"
          "text/html"
          "x-scheme-handler/about"
          "x-scheme-handler/chrome"
          "x-scheme-handler/ftp"
          "x-scheme-handler/http"
          "x-scheme-handler/https"
          "x-scheme-handler/irc"
          "x-scheme-handler/ircs"
          "x-scheme-handler/mailto"
          "x-scheme-handler/unknown"
          "x-scheme-handler/webcal"
        ];

      rbrowser = builtins.concatStringsSep
        "\n"
        (map (typ: "${typ}=rbrowser.desktop") mimeTypes);

    in ''
      [Default Applications]
      ${rbrowser}
    '';

in {
  options.shabka.workstation.enable = mkEnableOption "Workstation Profile";

  config = mkIf config.shabka.workstation.enable {
    services.flameshot.enable = true;
    services.network-manager-applet.enable = true;

    home = {
      packages = with pkgs; [
        nur.repos.kalbasit.rbrowser

        remmina

        weechat

        xsel

        # zoom for meetings
        # TODO: it's not building
        # zoom-us
      ];

      activation = {
        rbrowser-desktop-link = symlink
          "${pkgs.nur.repos.kalbasit.rbrowser}/share/applications/rbrowser.desktop"
          "${config.home.homeDirectory}/.local/share/applications/rbrowser.desktop";
      };

      file = {
        ".local/share/applications/mimeapps.list".text = mimeList;
        ".config/mimeapps.list".text = mimeList;
      };
    };

    programs.zsh.initExtra = ''
      # Set the browser to my relay browser
      export BROWSER="${pkgs.nur.repos.kalbasit.rbrowser}/bin/rbrowser"
    '';

    /*shabka.workstation = enableMultiple [
      "alacritty"
      "bluetooth"
      "dunst"
      "firefox"
      "greenclip"
      "gtk"
      "locker"
      "mysql-workbench"
      "rofi"
      "termite"
    ];*/
  };
}
