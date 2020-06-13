{ config, pkgs, lib, ... }:

with lib;

{
  options.shabka.workstation.firefox.enable = mkEnableOption "workstation.firefox";

  config = mkIf config.shabka.workstation.firefox.enable {
    programs.firefox = {
      enable = true;
    };

    home.file.".mozilla/firefox/profiles.ini".text = ''
      [General]
      StartWithLastProfile=1

      [Profile0]
      Name=personal
      IsRelative=1
      Path=profiles/personal

      [Profile2]
      Name=keeptruckin
      IsRelative=1
      Path=profiles/keeptruckin
    '';

    home.file.".mozilla/firefox/profiles/personal/.keep".text = "";
    home.file.".mozilla/firefox/profiles/keeptruckin/.keep".text = "";
  };
}
