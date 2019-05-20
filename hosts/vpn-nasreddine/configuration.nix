{ lib, ... }:

with lib;

{
  imports = [
    ./../../modules/nixos
  ]
  ++ (optionals (builtins.pathExists ./../../secrets/nixos) (singleton ./../../secrets/nixos));

  # set the default locale and the timeZone
  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "America/Los_Angeles";

  mine.hardware.machine = "cloud";
  mine.users.enable = false; # XXX: enable this for the root password but have to disable home-manager

  networking.hostName = "vpn-nasreddine";

  networking.firewall.allowedUDPPortRanges = [ { from = 1194; to = 1194; } ];
}
