{ config, pkgs, lib, ... }:

{
  services.redshift.enable = true;
  services.redshift.latitude = "34.42";
  services.redshift.longitude = "-122.11";
  services.redshift.temperature.day = 5900;
  services.redshift.temperature.night = 3700;
}
