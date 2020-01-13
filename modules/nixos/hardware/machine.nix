{ lib, ... }:

with lib;

{
  options.shabka.hardware.machine = mkOption {
    description = "The machine name (usually model).";
    default = null;
    type = with types;
      let
        supportedMachines = enum [
          "cloud"
          "hetzner_cloud"
          "hetzner_sb36"
          "hetzner_sb53"
          "hetzner-sb"
          "precision-7530"
          "thinkpad-e580"
          "xps-13"
          "zeus"
        ];
      in nullOr supportedMachines;
  };
}
