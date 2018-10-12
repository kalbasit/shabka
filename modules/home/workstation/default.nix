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

      # NOTE: Slack does not seem to find the rbrowser.desktop in
      #       ~/.nix-profile/share/applications so you must manually create a
      #       symlink to ~/.local/share/applications on bootstrap.
      # ln -s ../../../.nix-profile/share/applications/rbrowser.desktop ~/.local/share/applications/rbrowser.desktop
      rbrowser

      remmina

      weechat

      xsel

      # zoom for meetings
      zoom-us
    ];

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
