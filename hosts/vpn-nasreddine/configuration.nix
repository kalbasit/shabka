{ lib, ... }:

with lib;

{
  imports = optionals (builtins.pathExists ./../../secrets.nix) (singleton ./../../secrets.nix);

  # set the default locale and the timeZone
  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "America/Los_Angeles";

  networking.hostName = "vpn-nasreddine";

  networking.firewall.allowedUDPPortRanges = [ { from = 1194; to = 1194; } ];
}
