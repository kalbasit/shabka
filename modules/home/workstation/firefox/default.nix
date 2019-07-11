{ config, pkgs, lib, ... }:

with lib;

{
  options.shabka.workstation.firefox.enable = mkEnableOption "workstation.firefox";

  config = mkIf config.shabka.workstation.firefox.enable {
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
      Default=1

      [Profile1]
      Name=profiles/epita
      IsRelative=1
      Path=epita

      [Profile2]
      Name=lamacorp
      IsRelative=1
      Path=profiles/lamacorp
    '';

    home.file.".mozilla/firefox/profiles/epita/.keep".text = "";
    home.file.".mozilla/firefox/profiles/lamacorp/.keep".text = "";
    home.file.".mozilla/firefox/profiles/personal/.keep".text = "";
  };
}
