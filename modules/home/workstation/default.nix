{ config, pkgs, lib, ... }:

with lib;

with import ../../../util;

{
  options.mine.workstation.enable = mkEnableOption "Workstation Profile";

  config = mkIf config.mine.workstation.enable {
    services.flameshot.enable = true;
    services.kbfs.enable = true;
    services.keybase.enable = true;
    services.network-manager-applet.enable = true;

    home.packages = with pkgs; [
      keybase-gui

      rbrowser

      remmina

      weechat

      xsel

      # zoom for meetings
      zoom-us
    ];

    home.activation = {
      rbrowser-desktop-link = symlink
        "${pkgs.rbrowser}/share/applications/rbrowser.desktop"
        "${config.home.homeDirectory}/.local/share/applications/rbrowser.desktop";
    };

    programs.zsh.initExtra = ''
      # Set the browser to my relay browser
      export BROWSER="${pkgs.rbrowser}/bin/rbrowser"
    '';

    mine.workstation = enableMultiple [
      "alacritty"
      "chromium"
      "dropbox"
      "dunst"
      "firefox"
      "greenclip"
      "i3"
      "locker"
      "mysql-workbench"
      "rofi"
      "termite"
      "xsession"
    ];
  };
}
