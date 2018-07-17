{ config, pkgs, lib, ... }:

{
  services.redshift = {
    enable = true;
    latitude = lib.mkDefault "34.42";
    longitude = lib.mkDefault "-122.11";
    temperature.day = 5900;
    temperature.night = 3700;
  };
}
