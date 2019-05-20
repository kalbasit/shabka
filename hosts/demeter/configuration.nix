{ lib, ... }:

with lib;

{
  imports = [
    ../../modules/nixos

    ./home.nix
  ]
  ++ (optionals (builtins.pathExists ./../../secrets/nixos) (singleton ./../../secrets/nixos));

  boot.tmpOnTmpfs = true;

  # set the default locale and the timeZone
  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "America/Los_Angeles";

  networking.hostName = "demeter";

  mine.hardware.machine = "cloud";

  mine.users = {
    enable = true;

    users = {
      yl = { uid = 2000; isAdmin = true;  home = "/yl"; };
    };
  };

  mine.virtualisation.docker.enable = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?
}
