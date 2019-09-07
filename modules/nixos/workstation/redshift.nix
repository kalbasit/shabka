{ config, lib, ... }:

with lib;

{
  options.shabka.workstation.redshift.enable = mkEnableOption "workstation.redshift";

  config = mkIf config.shabka.workstation.redshift.enable {
    services.redshift.brightness.day = "1.0";
    services.redshift.brightness.night = "0.6";
    services.redshift.enable = true;
    services.redshift.temperature.day = 5900;
    services.redshift.temperature.night = 3700;
  };
}
