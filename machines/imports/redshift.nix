{ config, pkgs, lib, ... }:

{
  services.redshift.brightness.day = "1.0";
  services.redshift.brightness.night = "0.1";
  services.redshift.enable = true;
  services.redshift.latitude = "34.42";
  services.redshift.longitude = "-122.11";
  services.redshift.temperature.day = 5900;
  services.redshift.temperature.night = 3700;
}
