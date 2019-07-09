{ config, pkgs, lib, ... }:

with lib;

{
  options.shabka.workstation.sound.enable = mkEnableOption "workstation.sound";

  config = mkIf config.shabka.workstation.sound.enable {
    sound.enable = true;

    hardware = {
      pulseaudio = {
        enable = true;
        package = pkgs.pulseaudioFull;
      };
    };

    environment.systemPackages = with pkgs; [
      pavucontrol pa_applet
    ];
  };
}
