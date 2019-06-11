{ lib, ... }:

with lib;

let
  shabka = import <shabka> { };

in {
  imports = [
    ./hardware-configuration.nix

    "${shabka.external.nixos-hardware.path}/common/cpu/intel"
    "${shabka.external.nixos-hardware.path}/common/pc/laptop"
    "${shabka.external.nixos-hardware.path}/common/pc/laptop/ssd"

    ../../modules/nixos

    ./home.nix
  ];

  # set the default locale and the timeZone
  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "Europe/Paris";

  networking.hostName = "hedgehog";

  mine.hardware.intel_backlight.enable = true;
  mine.printing.enable = true;
  mine.users.enable = true;
  mine.virtualisation = {
    docker.enable = true;
    libvirtd.enable = true;
  };

  mine.workstation = {
    bluetooth.enable = true;
    fonts.enable = true;
    networking.enable = true;
    power.enable = true;
    sound.enable = true;
    teamviewer.enable = true;
    virtualbox.enable = true;
    xorg.enable = true;
  };

  mine.hardware.machine = "thinkpad-e580";

  services.logind.extraConfig = ''
    HandlePowerKey=suspend
  '';

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?
}
