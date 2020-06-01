{ config, pkgs, lib, ... }:

with lib;

{
  options.shabka.workstation.sound.enable = mkEnableOption "workstation.sound";

  config = mkIf config.shabka.workstation.sound.enable (mkMerge [
    {
      sound.enable = true;

      hardware = {
        pulseaudio.enable = true;
      };

      environment.systemPackages = with pkgs; [
        pavucontrol pa_applet
      ];
    }

    (mkIf config.shabka.workstation.bluetooth.enable {
      hardware.pulseaudio.package = pkgs.pulseaudioFull;
    })
  ]);
}
