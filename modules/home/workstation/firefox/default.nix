{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.workstation.firefox.enable = mkEnableOption "workstation.firefox";

  config = mkIf config.mine.workstation.firefox.enable {
    programs.firefox = {
      enable = true;
      enableGoogleTalk = true;
      enableAdobeFlash = true;
    };

    home.file.".mozilla/firefox/profiles.ini".text = ''
      [General]
      StartWithLastProfile=1

      [Profile0]
      Name=personal
      IsRelative=1
      Path=profiles/personal

      [Profile1]
      Name=publica
      IsRelative=1
      Path=profiles/publica

      [Profile2]
      Name=keeptruckin
      IsRelative=1
      Path=profiles/keeptruckin
    '';

    home.file.".mozilla/firefox/profiles/personal/.keep".text = "";
    home.file.".mozilla/firefox/profiles/publica/.keep".text = "";
    home.file.".mozilla/firefox/profiles/keeptruckin/.keep".text = "";
  };
}
