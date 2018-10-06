{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.workstation.sound.enable = mkEnableOption "workstation.sound";

  config = mkIf config.mine.workstation.sound.enable {
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

