{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.shabka.location;
in {
  options.shabka.location = {
    useGeoclue2 = mkEnableOption "Enable geoclue2 for geolocalization";
    
    latitude = mkOption {
      type = types.float;
      default = 34.42;
      description = ''
        Your current latitude, between
        <literal>-90.0</literal> and <literal>90.0</literal>. Must be provided
        along with longitude.
      '';
    };

    longitude = mkOption {
      type = types.float;
      default = -122.11;
      description = ''
        Your current longitude, between
        between <literal>-180.0</literal> and <literal>180.0</literal>. Must be
        provided along with latitude.
      '';
    };
  };

  config.location = {
    latitude = cfg.latitude;
    longitude = cfg.longitude;
    provider = if cfg.useGeoclue2 then "geoclue2" else "manual";
  };
}